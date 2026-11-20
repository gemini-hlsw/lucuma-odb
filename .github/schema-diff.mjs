//
// The parts of @graphql-inspector/action that validate-schema.mjs uses: the diff with annotations
// and the check summary. Copied from packages/action/helpers in
// https://github.com/graphql-hive/graphql-inspector (MIT), because the published package no longer
// contains these helpers (from version 6.2.2).
//

// @ts-check

import * as core from '@actions/core';
import {
  CriticalityLevel,
  diff as diffSchemas,
} from '@graphql-inspector/core';
import { getLocation, parse } from 'graphql';

/** @typedef {import('@graphql-inspector/core').Change} Change */
/** @typedef {import('graphql').GraphQLSchema} GraphQLSchema */
/** @typedef {import('graphql').Source} Source */
/** @typedef {import('graphql').SourceLocation} SourceLocation */

export const AnnotationLevel = Object.freeze({
  Failure: 'failure',
  Warning: 'warning',
  Notice: 'notice',
});

export const CheckConclusion = Object.freeze({
  Success: 'success',
  Neutral: 'neutral',
  Failure: 'failure',
});

/**
 * @typedef {object} Annotation
 * @property {string} path
 * @property {number} start_line
 * @property {number} end_line
 * @property {string} annotation_level
 * @property {string} message
 * @property {string} [title]
 * @property {number} [start_column]
 * @property {number} [end_column]
 */

/**
 * @typedef {object} ActionResult
 * @property {string} conclusion
 * @property {Annotation[]} [annotations]
 * @property {Change[]} [changes]
 */

/**
 * @param {{
 *   path: string,
 *   schemas: { old: GraphQLSchema, new: GraphQLSchema },
 *   source: Source,
 * }} input the schemas to compare, and the new schema file for the line numbers of the annotations
 * @returns {Promise<ActionResult>}
 */
export async function diff({ path, schemas, source }) {
  const changes = await diffSchemas(schemas.old, schemas.new, undefined, undefined, {
    onWarning: (warning) => core.warning(warning.message),
  });

  if (!changes?.length) {
    return { conclusion: CheckConclusion.Success };
  }

  const annotations = changes.map((change) =>
    annotate({ path, change, source }),
  );
  const conclusion = changes.some(
    (change) => change.criticality.level === CriticalityLevel.Breaking,
  )
    ? CheckConclusion.Failure
    : CheckConclusion.Success;

  return { conclusion, annotations, changes };
}

const levelMap = {
  [CriticalityLevel.Breaking]: AnnotationLevel.Failure,
  [CriticalityLevel.Dangerous]: AnnotationLevel.Warning,
  [CriticalityLevel.NonBreaking]: AnnotationLevel.Notice,
};

/**
 * @param {{ path: string, change: Change, source: Source }} input
 * @returns {Annotation}
 */
function annotate({ path, change, source }) {
  const loc = change.path
    ? getLocationByPath({ path: change.path, source })
    : { line: 1, column: 1 };

  return {
    title: change.message,
    annotation_level: levelMap[change.criticality.level],
    path,
    message: change.criticality.reason || change.message,
    start_line: loc.line,
    end_line: loc.line,
  };
}

/**
 * Finds the line of the type, field, argument, enum value or directive that a change path such as
 * `Query.user.id` or `@auth.role` points to.
 * @param {{ path: string, source: Source }} input
 * @returns {SourceLocation}
 */
function getLocationByPath({ path, source }) {
  const [typeName, ...rest] = path.split('.');
  const isDirective = typeName.startsWith('@');
  const doc = parse(source);

  /** @type {any} */
  let resolvedNode = undefined;

  for (const definition of doc.definitions) {
    if (
      isDirective &&
      definition.kind === 'DirectiveDefinition' &&
      definition.name.value === typeName.substring(1)
    ) {
      const [argName] = rest;
      resolvedNode =
        (argName &&
          definition.arguments?.find((arg) => arg.name.value === argName)) ||
        definition;
      break;
    }

    if (!('name' in definition) || definition.name?.value !== typeName) continue;

    switch (definition.kind) {
      case 'ObjectTypeDefinition':
      case 'InterfaceTypeDefinition':
        resolvedNode = rest[0] ? resolveField(rest, definition) : definition;
        break;
      case 'InputObjectTypeDefinition':
        resolvedNode = rest[0] ? resolveField([rest[0]], definition) : definition;
        break;
      case 'EnumTypeDefinition':
        resolvedNode =
          (rest[0] &&
            definition.values?.find((value) => value.name.value === rest[0])) ||
          definition;
        break;
      case 'UnionTypeDefinition':
      case 'ScalarTypeDefinition':
        resolvedNode = definition;
        break;
      default:
        continue;
    }
    break;
  }

  return resolveNodeSourceLocation(source, resolvedNode);
}

/**
 * @param {string[]} path field name, then an optional argument name
 * @param {any} definition an object, interface or input object type definition
 */
function resolveField([fieldName, argName], definition) {
  /** @type {any} */
  const field = definition.fields?.find(
    (/** @type {any} */ f) => f.name.value === fieldName,
  );
  if (!field) return definition;
  if (field.kind !== 'InputValueDefinition' && argName) {
    return (
      field.arguments?.find((/** @type {any} */ a) => a.name.value === argName) ||
      field
    );
  }
  return field;
}

/**
 * The line of the node itself, below its description.
 * @param {Source} source
 * @param {any} node
 * @returns {SourceLocation}
 */
function resolveNodeSourceLocation(source, node) {
  if (!node?.loc) return { line: 1, column: 1 };

  const nodeLocation = getLocation(source, node.loc.start);
  if (node.description?.loc) {
    return {
      line: getLocation(source, node.description.loc.end).line + 1,
      column: nodeLocation.column,
    };
  }
  return nodeLocation;
}

/**
 * Wraps quoted names in bold Markdown.
 * @param {string} msg
 */
function bolderize(msg) {
  /** @param {string} _ @param {string} value */
  const bold = (_, value) => `**${value}**`;
  return msg.replace(/'([^']+)'/gim, bold).replace(/"([^"]+)"/gim, bold);
}

/**
 * Markdown summary of the changes, grouped by criticality.
 * @param {Change[]} changes
 * @param {number} summaryLimit the maximum number of changes to list
 */
export function createSummary(changes, summaryLimit) {
  /** @param {string} level */
  const byLevel = (level) =>
    changes.filter((change) => change.criticality.level === level);
  const breaking = byLevel(CriticalityLevel.Breaking);
  const dangerous = byLevel(CriticalityLevel.Dangerous);
  const safe = byLevel(CriticalityLevel.NonBreaking);

  const summary = [
    `# Found ${changes.length} change${changes.length > 1 ? 's' : ''}`,
    '',
    `Breaking: ${breaking.length}`,
    `Dangerous: ${dangerous.length}`,
    `Safe: ${safe.length}`,
  ];

  if (changes.length > summaryLimit) {
    summary.push(
      '',
      `Total amount of changes (${changes.length}) is over the limit (${summaryLimit})`,
      '',
    );
  }

  /** @param {string} type @param {Change[]} group */
  function addChangesToSummary(type, group) {
    if (group.length === 0) return;
    if (group.length <= summaryLimit) {
      summary.push(
        '',
        `## ${type} changes`,
        ...group.map((change) => ` - ${bolderize(change.message)}`),
      );
    }
    summaryLimit -= group.length;
  }

  addChangesToSummary('Breaking', breaking);
  addChangesToSummary('Dangerous', dangerous);
  addChangesToSummary('Safe', safe);

  return summary.join('\n');
}
