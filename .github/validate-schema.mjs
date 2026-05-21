//
// This file has _most_ of the functionality of the validate-schema action, but the main difference with the original action is that it supports loading schema files from JVM resource directories (in the same project). This is done by the LucumaResourceLoader
// Annotations are supported and added during GitHub actions check using GHA annotations.
// Resource import statements are used by schemas like resource.graphql that import from `"lucuma/odb/graphql/OdbSchema.graphql"`.
// Most of the code is taken from https://github.com/graphql-hive/graphql-inspector
//

// @ts-check

import * as core from '@actions/core';
import * as github from '@actions/github';
import { diff } from '@graphql-inspector/action/action/helpers/diff';
import { CheckConclusion } from '@graphql-inspector/action/action/helpers/types';
import { createSummary } from '@graphql-inspector/action/action/helpers/utils';
import {
  getAssociatedPullRequest,
  getCurrentCommitSha,
} from '@graphql-inspector/action/git';
import { batch } from '@graphql-inspector/action/utils';
import { GitLoader } from '@graphql-tools/git-loader';
import { loadFromGit } from '@graphql-tools/git-loader/load-git';
import { GraphQLFileLoader } from '@graphql-tools/graphql-file-loader';
import { processImport } from '@graphql-tools/import';
import { loadSchema } from '@graphql-tools/load';
import { parseGraphQLSDL } from '@graphql-tools/utils';
import fs from 'fs';
import { readFile } from 'fs/promises';
import { glob } from 'glob';
import { printSchema, Source } from 'graphql';

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

// Map of resource path to content, used for handling #import statements in .graphql files
const resourceLocations = await glob('**/src/main/resources/**/*.graphql');
const resourceMapping = Object.fromEntries(
  await Promise.all(
    resourceLocations.map(async (l) => [
      l.split('src/main/resources/')[1],
      await readFile(l, 'utf-8'),
    ]),
  ),
);

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

/**
 * From @graphql-tools/graphql-file-loader, internal function used by handleFileContent
 * @param {string} rawSDL
 */
function isGraphQLImportFile(rawSDL) {
  const trimmedRawSDL = rawSDL.trim();
  return (
    trimmedRawSDL.startsWith('# import') || trimmedRawSDL.startsWith('#import')
  );
}

class LucumaResourceLoader extends GraphQLFileLoader {
  /**
   * Same as the super, but passes along resourceMapping as predefinedImports
   * @param {string} rawSDL
   * @param {string} pointer
   * @param {import('@graphql-tools/graphql-file-loader').GraphQLFileLoaderOptions} options
   * @returns
   */
  handleFileContent(rawSDL, pointer, options) {
    if (!options.skipGraphQLImport && isGraphQLImportFile(rawSDL)) {
      const document = processImport(pointer, options.cwd, resourceMapping);
      return {
        location: pointer,
        document,
      };
    }
    return parseGraphQLSDL(pointer, rawSDL, options);
  }
}

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
      return su.map(({ location }) => {
        const r = processImport(location, options.cwd, gitResourceMapping);
        return {
          location,
          document: r,
        };
      });
    }
    return su;
  }
}

const schemaLoaderOptions = {
  loaders: [new LucumaResourceLoader(), new LucumaResourceGitLoader()],
};
const newSchema = await loadSchema(schemaPath, schemaLoaderOptions);
const oldSchema = await loadSchema(
  `git:origin/main:${schemaPath}`,
  schemaLoaderOptions,
);

const action = await diff({
  path: schemaPath,
  schemas: {
    old: oldSchema,
    new: newSchema,
  },
  sources: {
    old: new Source(printSchema(oldSchema), `main:${schemaPath}`),
    new: new Source(printSchema(newSchema), schemaPath),
  },
});

async function reportToGithub() {
  const commitSha = getCurrentCommitSha();
  const token = process.env.GITHUB_TOKEN;
  /**
   * @type {import('@graphql-inspector/action/types').OctokitInstance}
   */
  // @ts-ignore
  const octokit = github.getOctokit(token);
  const pullRequest = await getAssociatedPullRequest(octokit, commitSha);

  const checkName = core.getInput('name') || 'GraphQL Inspector';
  const { owner, repo } = github.context.repo;

  core.info(`Creating a check named "${checkName}"`);
  const check = await octokit.rest.checks.create({
    owner,
    repo,
    name: checkName,
    head_sha: commitSha,
    status: 'in_progress',
  });
  const checkId = check.data.id;

  let conclusion = action.conclusion;
  const changes = action.changes ?? [];
  const annotations = action.annotations || [];

  const hasApprovedBreakingChangeLabel = pullRequest?.labels?.some(
    (label) => label.name === APPROVE_LABEL,
  );
  if (hasApprovedBreakingChangeLabel) {
    conclusion = CheckConclusion.Success;
  }

  const summary = createSummary(changes, 100, false);

  const title =
    conclusion === CheckConclusion.Failure
      ? 'Something is wrong with your schema'
      : 'Everything looks good';
  core.info(`Conclusion: ${conclusion}`);

  await updateCheckRun(octokit, checkId, {
    conclusion,
    output: { title, summary, annotations },
  });
}

/**
 * @type {typeof import('@graphql-inspector/action/checks').updateCheckRun}
 */
const updateCheckRun = async (octokit, checkId, { conclusion, output }) => {
  core.info(`Updating check: ${checkId}`);
  const { title, summary, annotations = [] } = output;
  const batches = batch(annotations, 50);
  core.info(`annotations to be sent: ${annotations.length}`);
  await octokit.rest.checks.update({
    check_run_id: checkId,
    completed_at: new Date().toISOString(),
    status: 'completed',
    ...github.context.repo,
    conclusion,
    output: {
      title,
      summary,
    },
  });
  try {
    await Promise.all(
      batches.map(async (chunk) => {
        await octokit.rest.checks.update({
          check_run_id: checkId,
          ...github.context.repo,
          output: {
            title,
            summary,
            annotations: chunk,
          },
        });
        core.info(`annotations sent (${chunk.length})`);
      }),
    );
  } catch (error) {
    core.error(`failed to send annotations: ${error}`);
    throw error;
  }
  // Fail
  if (conclusion === CheckConclusion.Failure) {
    core.error(
      'Schema validation failed:\n' +
        output.annotations?.map((c) => `${c.path}: ${c.message}`).join('\n'),
    );
    return core.setFailed(output.title ?? 'Check failed');
  }
  // Success or Neutral
};

if (process.env.GITHUB_ACTIONS) {
  reportToGithub();
} else if (action.conclusion === CheckConclusion.Failure) {
  console.error(
    'Schema validation failed:\n' +
      action.changes?.map((c) => `${c.path}: ${c.message}`).join('\n'),
  );
  process.exit(1);
}
