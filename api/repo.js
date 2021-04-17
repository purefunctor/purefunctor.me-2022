"use strict";

const got = require("got");

require("dotenv").config();

function getRepositoryUrl(owner, repository) {
  return `https://api.github.com/repos/${owner}/${repository}`;
};

async function getRepositoryData(owner, repository) {
  const baseUrl = getRepositoryUrl(owner, repository);
  const languagesUrl = baseUrl + "/languages";
  const participationUrl = baseUrl + "/stats/participation";

  const gh = async (url) => {
    return got(url, {
      username: process.env.GITHUB_USER,
      password: process.env.GITHUB_SECRET,
    }).then(
      value => JSON.parse(value.body)
    );
  };

  const basic = await gh(baseUrl);
  const languages = await gh(languagesUrl);
  const participation = await gh(participationUrl);

  return {
    description: basic["description"],
    stars: basic["stargazers_count"],
    commits: participation["all"],
    languages: languages,
  };
};

module.exports = (_, res) => {
  getRepositoryData("PureFunctor", "purefunctor.me").then(
    payload => res.json(payload)
  ).catch(
    () => res.json({"error": "an error was encountered!"})
  );
};
