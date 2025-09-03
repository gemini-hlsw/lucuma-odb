# Deployment

Whenever a PR is merged into `main`, a Github Action is triggered that builds Docker images for all the applications and pushes them them to Heroku.

For more information see [Heroku's docs on Docker deployment](https://devcenter.heroku.com/articles/container-registry-and-runtime).

Deploying a Docker image to Heroku is a 3-step process:
1) Tagging the image.
2) Pushing the image.
3) Releasing the image as an app.

The images are tagged and pushed (steps 1 and 2) to all environments (`dev`, `staging` and `production`), but they are only released (step 3) for `dev`:

Since Heroku does not support promotion for applications deployed via Docker, this provides a fast way of releasing the latest version to `staging` or `production`. This is usually done via de `promote.sh` script in the [`explore`](https://github.com/gemini-hlsw/explore) repo, which performs a backup of the databases and ensures all systems are in sync in a given environment.