const { Octokit: GitHubOctokit } = require("@octokit/rest");
const { retry } = require("@octokit/plugin-retry");
const { throttling } = require("@octokit/plugin-throttling");
const Octokit = GitHubOctokit.plugin(retry, throttling);

exports.mkOctokitImpl = function (authToken) {
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

exports.requestImpl = function (octokit, route, headers, args, onError, onSuccess) {
  args["headers"] = headers;
  return octokit
    .request(route, args)
    .then((data) => onSuccess(data))
    .catch((err) => onError(err));
};

exports.paginateImpl = function (octokit, route, headers, args, onError, onSuccess) {
  args["headers"] = headers;
  return octokit
    .paginate(route, args)
    .then((data) => onSuccess(data))
    .catch((err) => onError(err));
};
