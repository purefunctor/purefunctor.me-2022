"use strict";

const got = require("got");

function getRepositoryUrl(owner, repository) {
  return `https://api.github.com/repos/${owner}/${repository}`;
}

async function getRepositoryData(owner, repository) {
  const baseUrl = getRepositoryUrl(owner, repository);
  const languagesUrl = baseUrl + "/languages";
  const participationUrl = baseUrl + "/stats/participation";

  const basic = await got(baseUrl).then(
    value => JSON.parse(value.body)
  );
  const languages = await got(languagesUrl).then(
    value => JSON.parse(value.body)
  );
  const participation = await got(participationUrl).then(
    value => JSON.parse(value.body)
  );

  return {
    description: basic["description"],
    languages: languages,
    stars: basic["stargazers_count"],
    commits: participation["all"],
  };
}

module.exports = (_, res) => {
  getRepositoryData("PureFunctor", "purefunctor.me").then(
    payload => res.json(payload)
  ).catch(
    () => res.json({"error": "an error was encountered!"})
  );
};
