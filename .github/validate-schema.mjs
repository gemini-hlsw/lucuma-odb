//
// This file has _most_ of the functionality of the validate-schema action, but the main difference with the original action is that it supports loading schema files from JVM resource directories (in the same project). This is done with path aliases for local files and by the LucumaResourceGitLoader for files on main.
// Annotations are emitted as workflow commands and the step fails through its exit code, so no GitHub API call is involved.
// Resource import statements are used by schemas like resource.graphql that import from `"lucuma/odb/graphql/OdbSchema.graphql"`.
// Most of the code is taken from https://github.com/graphql-hive/graphql-inspector
//

// @ts-check

import * as core from '@actions/core';
import { GitLoader } from '@graphql-tools/git-loader';
import { loadFromGit } from '@graphql-tools/git-loader/load-git';
import { GraphQLFileLoader } from '@graphql-tools/graphql-file-loader';
import { processImport } from '@graphql-tools/import';
import { loadSchema } from '@graphql-tools/load';
import fs from 'fs';
import { glob } from 'glob';
import { Source } from 'graphql';
import {
  AnnotationLevel,
  CheckConclusion,
  createSummary,
  diff,
} from './schema-diff.mjs';
const APPROVE_LABEL = 'expected-breaking-change';

const schemaPath = process.argv[2];
if (!schemaPath) {
  console.error('Provide a schema path as an argument');
  process.exit(1);
}
if (!fs.existsSync(schemaPath)) {
  console.error(`Schema file ${schemaPath} does not exist`);
  process.exit(1);
}

// The #import statements in .graphql files use paths relative to the resource directories.
const resourceLocations = await glob('**/src/main/resources/**/*.graphql');
const resourcePathAliases = {
  mappings: Object.fromEntries(
    resourceLocations.map((l) => [l.split('src/main/resources/')[1], `./${l}`]),
  ),
};

const gitResourceMapping = Object.fromEntries(
  (
    await Promise.all(
      resourceLocations.map(async (l) => {
        const gitContent = await loadFromGit({
          ref: 'origin/main',
          path: l,
        });
        return [
          // Provide both the full git path and path relative to resources
          [`git:origin/main:${l}`, gitContent],
          [l.split('src/main/resources/')[1], gitContent],
        ];
      }),
    )
  ).flat(),
);

// @ts-ignore
class LucumaResourceGitLoader extends GitLoader {
  /**
   *
   * @param {string} pointer
   * @param {import('@graphql-tools/git-loader').GitLoaderOptions} options
   * @returns
   */
  async handleSingularPointerAsync(pointer, options) {
    /**
     * override the super method to add import processing to git-loaded files.
     * @type {{location: string}[] | null}
     */
    // @ts-ignore
    const su = await super.handleSingularPointerAsync(pointer, options);
    if (su) {
      return su.map(({ location }) => ({
        location,
        document: processImport(location, options.cwd, gitResourceMapping),
      }));
    }
    return su;
  }
}

const schemaLoaderOptions = {
  loaders: [new GraphQLFileLoader(), new LucumaResourceGitLoader()],
  pathAliases: resourcePathAliases,
};
const newSchema = await loadSchema(schemaPath, schemaLoaderOptions);
const oldSchemaPath = `git:origin/main:${schemaPath}`;
const oldSchema = await loadSchema(oldSchemaPath, schemaLoaderOptions);

const action = await diff({
  path: schemaPath,
  schemas: {
    old: oldSchema,
    new: newSchema,
  },
  source: new Source(fs.readFileSync(schemaPath, 'utf-8'), schemaPath),
});

const annotations =
  action.annotations?.filter(
    (c) => c.annotation_level !== AnnotationLevel.Notice,
  ) ?? [];

/**
 * Reads the labels live, so that re-running the job after adding the label is enough: a re-run
 * reuses the original event payload, where the label is still missing.
 * Falls back to the payload when GitHub can't be reached.
 * @returns {Promise<string[]>}
 */
async function pullRequestLabels() {
  const eventPath = process.env.GITHUB_EVENT_PATH;
  if (!eventPath || !fs.existsSync(eventPath)) return [];
  const pullRequest = JSON.parse(fs.readFileSync(eventPath, 'utf-8')).pull_request;
  if (!pullRequest) return [];
  const names = (/** @type {{name: string}[]} */ labels) =>
    labels.map((label) => label.name);
  try {
    const response = await fetch(
      `${process.env.GITHUB_API_URL}/repos/${process.env.GITHUB_REPOSITORY}/issues/${pullRequest.number}/labels?per_page=100`,
      {
        headers: {
          accept: 'application/vnd.github+json',
          ...(process.env.GITHUB_TOKEN
            ? { authorization: `Bearer ${process.env.GITHUB_TOKEN}` }
            : {}),
        },
        signal: AbortSignal.timeout(10_000),
      },
    );
    if (!response.ok) throw new Error(`HTTP ${response.status}`);
    return names(await response.json());
  } catch (error) {
    core.warning(
      `Could not read the current labels (${error}), using the ones in the event payload`,
    );
    return names(pullRequest.labels ?? []);
  }
}

// Only a failure needs the labels, so a passing check makes no request at all.
const approved =
  action.conclusion === CheckConclusion.Failure &&
  (await pullRequestLabels()).includes(APPROVE_LABEL);
const failed = action.conclusion === CheckConclusion.Failure && !approved;

if (process.env.GITHUB_ACTIONS) {
  for (const annotation of annotations) {
    const properties = {
      title: annotation.title,
      file: annotation.path,
      startLine: annotation.start_line,
      endLine: annotation.end_line,
    };
    if (annotation.annotation_level === AnnotationLevel.Failure && !approved)
      core.error(annotation.message, properties);
    else core.warning(annotation.message, properties);
  }
  await core.summary
    .addHeading(`GraphQL Inspector: ${schemaPath}`, 3)
    .addEOL()
    .addRaw(createSummary(action.changes ?? [], 100), true)
    .write();
  if (approved)
    core.info(`Breaking changes accepted by the "${APPROVE_LABEL}" label`);
  if (failed) core.setFailed('Something is wrong with your schema');
} else {
  if (annotations.length > 0)
    console.error(
      (failed ? 'Schema validation failed:\n' : 'Schema changes:\n') +
        annotations
          .map(
            (c) =>
              `${c.path}:${c.start_line}${c.start_column ? ':' + c.start_column : ''}: ${c.message}`,
          )
          .join('\n'),
    );
  if (failed) process.exit(1);
}
