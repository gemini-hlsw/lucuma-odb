const fs = require('fs');
const path = require('path');
const { 
  buildSchema, 
  printSchema, 
  GraphQLSchema,
  GraphQLInputObjectType,
  parse,
  visit,
  Kind,
  buildASTSchema,
  print,
  isInputObjectType,
  isObjectType,
  isInterfaceType,
  isUnionType,
  isEnumType,
  isScalarType,
  getNullableType,
  getNamedType,
} = require('graphql');

// Parse command line arguments
function parseArguments() {
  if (process.argv.length < 5) {
    console.error('Usage: node script.js <schema1File> <schema2File> <outputFile>');
    process.exit(1);
  }
  
  return {
    schema1Path: process.argv[2],
    schema2Path: process.argv[3],
    outputPath: process.argv[4],
    debug: process.argv.includes('--debug')
  };
}

const args = parseArguments();
const DEBUG = args.debug;

// Function to read schema files
function readSchema(filePath) {
  try {
    return fs.readFileSync(path.resolve(filePath), 'utf8');
  } catch (error) {
    console.error(`Error reading file ${filePath}:`, error.message);
    process.exit(1);
  }
}

// 1. Read both schema files
console.log(`Reading schema files: ${args.schema1Path} and ${args.schema2Path}`);
const schema1Raw = readSchema(args.schema1Path);
const schema2Raw = readSchema(args.schema2Path);

// 2. First, merge the schemas as-is to preserve all type references
console.log('First merging schemas to preserve all type references...');
const initialMergedSchemaSDL = schema1Raw + '\n' + schema2Raw;
let mergedSchema;

try {
  // Attempt to build schema - might fail if there are conflicting operation types
  mergedSchema = buildASTSchema(parse(initialMergedSchemaSDL));
} catch (error) {
  console.log('Encountered schema merge conflict. Removing operation types from schema1 first...');
  
  // If direct merge fails, we need to remove operation types from schema1
  const ast = parse(schema1Raw);
  
  const modifiedAst = visit(ast, {
    SchemaDefinition(node) {
      // Remove operation types from schema definition
      return {
        ...node,
        operationTypes: []
      };
    },
    ObjectTypeDefinition(node) {
      // Remove Query, Mutation and Subscription type definitions
      if (
        node.name.value === 'Query' || 
        node.name.value === 'Mutation' || 
        node.name.value === 'Subscription'
      ) {
        return null;
      }
      return node;
    }
  });
  
  // Convert back to SDL string
  const schema1Modified = print(modifiedAst);
  
  // Try merging again with modified schema1
  const modifiedMergedSchemaSDL = schema1Modified + '\n' + schema2Raw;
  mergedSchema = buildASTSchema(parse(modifiedMergedSchemaSDL));
}

// 3. Identify entry points (Query, Mutation, Subscription) in the merged schema
const queryType = mergedSchema.getQueryType();
const mutationType = mergedSchema.getMutationType();
const subscriptionType = mergedSchema.getSubscriptionType();

const entryPoints = [queryType, mutationType, subscriptionType].filter(Boolean);

if (entryPoints.length === 0) {
  console.error('Error: No operation types (Query, Mutation, Subscription) found in the merged schema.');
  console.error('The pruning process requires at least one operation type as an entry point.');
  process.exit(1);
}

console.log(`Found operation types: ${entryPoints.map(t => t.name).join(', ')}`);

// 4. Build dependency graph and identify reachable types with improved input handling
function buildDependencyGraph(schema) {
  const graph = {};
  const typeMap = schema.getTypeMap();
  
  // Initialize graph with all types
  Object.values(typeMap).forEach(type => {
    if (!type.name.startsWith('__')) {
      graph[type.name] = new Set();
    }
  });
  
  // First pass: Add dependencies for all types
  Object.values(typeMap).forEach(type => {
    // Skip built-in types and introspection types
    if (type.name.startsWith('__')) {
      return;
    }
    
    // Handle Object and Interface types
    if (isObjectType(type) || isInterfaceType(type)) {
      const fields = type.getFields();
      Object.values(fields).forEach(field => {
        // Add field type as dependency
        addTypeDependency(graph, type.name, getNamedType(field.type).name);
        
        // Process field arguments (important for linking to input types)
        if (field.args && field.args.length > 0) {
          field.args.forEach(arg => {
            addTypeDependency(graph, type.name, getNamedType(arg.type).name);
          });
        }
      });
    }

    // Add dependency from ObjectType to implemented Interfaces
    if (isObjectType(type)) {
      const interfaces = type.getInterfaces();
      if (interfaces && interfaces.length > 0) {
        interfaces.forEach(iface => {
          addTypeDependency(graph, type.name, iface.name);
        });
      }
    }
    
    // Handle Input Object types
    if (isInputObjectType(type)) {
      const fields = type.getFields();
      Object.values(fields).forEach(field => {
        addTypeDependency(graph, type.name, getNamedType(field.type).name);
      });
    }
    
    // Handle Union types
    if (isUnionType(type)) {
      type.getTypes().forEach(memberType => {
        addTypeDependency(graph, type.name, memberType.name);
      });
    }
  });
  
  // Second pass: Add reverse dependencies for input types
  // This ensures inputs are marked as reachable when they're used in arguments
  Object.values(typeMap).forEach(type => {
    if (type.name.startsWith('__')) return;
    
    if (isObjectType(type) || isInterfaceType(type)) {
      const fields = type.getFields();
      Object.values(fields).forEach(field => {
        const fieldType = getNamedType(field.type);
        // Add a reverse dependency - the field's type depends on the parent type
        addTypeDependency(graph, fieldType.name, type.name);

        // For each argument that uses an input type, add a reverse dependency
        if (field.args && field.args.length > 0) {
          field.args.forEach(arg => {
            const argType = getNamedType(arg.type);
            if (isInputObjectType(argType)) {
              // Add a reverse dependency - the field's parent type depends on this input type
              addTypeDependency(graph, argType.name, type.name);
            }
          });
        }
      });
    }
  });
  
  // Convert sets to arrays for easier handling
  Object.keys(graph).forEach(typeName => {
    graph[typeName] = Array.from(graph[typeName]);
  });
  
  return graph;
}

function addTypeDependency(graph, fromType, toType) {
  if (graph[fromType]) {
    graph[fromType].add(toType);
  }
}

console.log('Building dependency graph with improved input type handling...');
const dependencyGraph = buildDependencyGraph(mergedSchema);

if (DEBUG) {
  fs.writeFileSync('dependency-graph.json', JSON.stringify(dependencyGraph, null, 2));
  console.log('Wrote dependency graph to dependency-graph.json for debugging');
}

// 5. Find all reachable types by traversing in both directions
function findReachableTypes(graph, startNodes) {
  const visited = new Set();
  const queue = [];
  
  // Initialize queue with start nodes
  startNodes.forEach(type => {
    if (type) {
      queue.push(type.name);
      visited.add(type.name);
    }
  });
  
  // BFS through the graph - forward pass
  while (queue.length > 0) {
    const current = queue.shift();
    
    if (graph[current]) {
      graph[current].forEach(neighbor => {
        if (!visited.has(neighbor)) {
          visited.add(neighbor);
          queue.push(neighbor);
        }
      });
    }
  }
  
  // Additional scan for input objects used in arguments
  // of reachable types' fields
  const typeMap = mergedSchema.getTypeMap();
  let foundNew = true;
  
  // Keep scanning until we stop finding new reachable types
  while (foundNew) {
    foundNew = false;
    
    Object.values(typeMap).forEach(type => {
      // Skip non-reachable, built-in, and non-object types
      if (!visited.has(type.name) || type.name.startsWith('__') || 
          (!isObjectType(type) && !isInterfaceType(type))) {
        return;
      }
      
      // Check fields of reachable object types
      const fields = type.getFields();
      Object.values(fields).forEach(field => {
        // Check arguments of fields for input types
        if (field.args && field.args.length > 0) {
          field.args.forEach(arg => {
            const argType = getNamedType(arg.type);
            if (!visited.has(argType.name)) {
              visited.add(argType.name);
              queue.push(argType.name);
              foundNew = true;
            }
          });
        }
      });
    });
    
    // Process queue if we found new nodes
    while (queue.length > 0) {
      const current = queue.shift();
      
      if (graph[current]) {
        graph[current].forEach(neighbor => {
          if (!visited.has(neighbor)) {
            visited.add(neighbor);
            queue.push(neighbor);
            foundNew = true;
          }
        });
      }
    }
  }
  
  return visited;
}

console.log('Finding reachable types with bidirectional traversal...');
const reachableTypes = findReachableTypes(dependencyGraph, entryPoints);
console.log(`Found ${reachableTypes.size} reachable types.`);

// 6. Generate pruned schema
function pruneSchema(schema, reachableTypes) {
  const typeMap = schema.getTypeMap();
  const prunedTypes = {};
  const keptTypeNames = [];
  const removedTypeNames = [];
  
  // Keep only reachable types and built-in types
  Object.entries(typeMap).forEach(([name, type]) => {
    if (
      reachableTypes.has(name) || 
      name.startsWith('__')
    ) {
      prunedTypes[name] = type;
      if (!name.startsWith('__')) {
        keptTypeNames.push(name);
      }
    } else if (!name.startsWith('__')) {
      removedTypeNames.push(name);
    }
  });
  
  // Construct new schema with only reachable types
  return {
    schema: new GraphQLSchema({
      query: schema.getQueryType(),
      mutation: schema.getMutationType(),
      subscription: schema.getSubscriptionType(),
      types: Object.values(prunedTypes)
    }),
    keptTypeNames,
    removedTypeNames
  };
}

console.log('Pruning schema...');
const { schema: prunedSchema, keptTypeNames, removedTypeNames } = pruneSchema(mergedSchema, reachableTypes);

// 7. Write output to file
let outputSDL = printSchema(prunedSchema);

// Add comment to the beginning of the output file
outputSDL = `
"""
This file is generated. Do not manually modify it.
It is generated from the following source files:
  - ${path.basename(args.schema1Path)}
  - ${path.basename(args.schema2Path)}
"""
${outputSDL}
`;

try {
  fs.writeFileSync(args.outputPath, outputSDL);
  console.log(`Success! Pruned and merged schema has been written to ${args.outputPath}`);
  
  // Report detailed statistics
  console.log('\nSchema pruning statistics:');
  console.log(`- Total types processed: ${keptTypeNames.length + removedTypeNames.length}`);
  console.log(`- Kept types: ${keptTypeNames.length}`);
  console.log(`- Removed types: ${removedTypeNames.length} (${Math.round(removedTypeNames.length / (keptTypeNames.length + removedTypeNames.length) * 100)}%)`);
  
  // Count by type category
  const inputsKept = keptTypeNames.filter(name => 
    isInputObjectType(mergedSchema.getType(name))).length;
  const inputsRemoved = removedTypeNames.filter(name => 
    isInputObjectType(mergedSchema.getType(name))).length;
    
  console.log(`- Input types kept: ${inputsKept}`);
  console.log(`- Input types removed: ${inputsRemoved}`);
  
  if (removedTypeNames.length > 0 && removedTypeNames.length <= 50) {
    console.log('\nRemoved types:');
    removedTypeNames.sort().forEach(name => {
      const type = mergedSchema.getType(name);
      const typeCategory = 
        isInputObjectType(type) ? 'Input' :
        isObjectType(type) ? 'Object' :
        isInterfaceType(type) ? 'Interface' :
        isUnionType(type) ? 'Union' :
        isEnumType(type) ? 'Enum' :
        isScalarType(type) ? 'Scalar' : 'Unknown';
      
      console.log(`  - ${name} (${typeCategory})`);
    });
  } else if (removedTypeNames.length > 50) {
    console.log(`\nRemoved types: ${removedTypeNames.length} types (too many to list)`);
    console.log('Use --debug flag to write full list to file');
  }
  
  if (DEBUG && removedTypeNames.length > 0) {
    fs.writeFileSync('removed-types.json', JSON.stringify({
      removed: removedTypeNames.sort(),
      kept: keptTypeNames.sort()
    }, null, 2));
    console.log('Wrote removed and kept type lists to removed-types.json for debugging');
  }
  
  console.log('\nOperation types in final schema:');
  if (prunedSchema.getQueryType()) console.log(`  - Query: ${prunedSchema.getQueryType().name}`);
  if (prunedSchema.getMutationType()) console.log(`  - Mutation: ${prunedSchema.getMutationType().name}`);
  if (prunedSchema.getSubscriptionType()) console.log(`  - Subscription: ${prunedSchema.getSubscriptionType().name}`);
} catch (error) {
  console.error(`Error writing to output file ${args.outputPath}:`, error.message);
  process.exit(1);
}
