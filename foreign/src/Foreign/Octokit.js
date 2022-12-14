import { Octokit as GitHubOctokit } from "@octokit/rest";
import { retry } from "@octokit/plugin-retry";
import { throttling } from "@octokit/plugin-throttling";

const Octokit = GitHubOctokit.plugin(retry, throttling);

export const newOctokitImpl = (authToken) => {
  const octokit = new Octokit({
    auth: authToken,
    request: {
      per_page: 100, // this is the maximum
    },
    throttle: {
      onRateLimit: (retryAfter, options) => {
        octokit.log.warn(
          `Request quota exhausted for request ${options.method} ${options.url}`
        );

        // Retry twice after hitting a rate limit error, then give up
        if (options.request.retryCount <= 2) {
          console.log(`Retrying after ${retryAfter} seconds!`);
          return true;
        }
      },
      onAbuseLimit: (_, options) => {
        octokit.log.error(
          `Abuse detected for request ${options.method} ${options.url}`
        );
      },
    },
  });
  return octokit;
};

export function requestImpl(octokit, route, headers, args, onError, onSuccess) {
  args["headers"] = headers;
  return octokit
    .request(route, args)
    .then((data) => onSuccess(data))
    .catch((err) => onError(err));
}

export function paginateImpl(
  octokit,
  route,
  headers,
  args,
  onError,
  onSuccess
) {
  args["headers"] = headers;
  return octokit
    .paginate(route, args)
    .then((data) => onSuccess(data))
    .catch((err) => onError(err));
}
