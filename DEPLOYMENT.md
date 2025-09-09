# Deployment

## TLDR;

> **I've merged an ODB PR with a bug fix or new feature. What do I need to do to make it the current dev version running on Heroku?**

Nothing. The application is deployed to development and restarted automatically.

> **We're happy with the way it is working in development, how do I make it the current version running in staging? In production?**

Run the promote.sh script in the explore application with these arguments:
- `promote.sh dev staging`
- `promote.sh staging production`
- YOLO: `promote.sh dev production`

## More than you want to know

Whenever a PR is merged into `main`, a Github Action is triggered that builds Docker images for all the applications and pushes them them to Heroku (for more information on this see [Heroku's docs on Docker deployment](https://devcenter.heroku.com/articles/container-registry-and-runtime)).

Deploying a Docker image to Heroku is a 3-step process:
1) Tagging the image specifically for an application and process type.
2) Pushing the image to Heroku's Container Registry.
3) Releasing the image. This restarts the application. Until this is done, the application will continue to run the previously release version.

The images are tagged and pushed (steps 1 and 2) to all environments (`dev`, `staging` and `production`), but they are only released (step 3) for `dev`. Furthermore, a deployment record is created in GitHub (see [GitHub's docs on deployments](https://docs.github.com/en/rest/deployments/deployments)). The unique ids of the Docker images is added as extra information (`payload`) to the deployment record.

Heroku does not support promotion for applications deployed via Docker, we use our own mechanism. This is implemented in the `promote.sh` script in the [`explore`](https://github.com/gemini-hlsw/explore) repo. The script is provided with a source environment and a target environment. When executed, it reads the deployment record of the source deployment, from which it can extract the unique ids of the Docker images in the source environment and release them in the target environment. Furthermore, the script makes a backup of the databases in the target environment. It will also promote Explore in Firebase.