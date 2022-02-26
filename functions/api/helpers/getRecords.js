const Airtable = require("airtable");
const https = require("https");
const queryString = require("querystring");

Airtable.configure({
  endpointUrl: process.env.API_URL,
  apiKey: process.env.API_KEY
});

const formattedReturn = require("./formattedReturn");
module.exports = async event => {
  try {
    const table = event.queryStringParameters.base;
    const view = event.queryStringParameters.view;
    if (
      table == "users" &&
      view != "VKS_output" &&
      view != "SPR_output" &&
      view != "SentenceCompletion_output"
    ) {
      throw Error("You can't access users' table(please?)");
    }
    if (event.queryStringParameters.outputRequest) {
      return outputRequest(event);
    }
    const base = Airtable.base(event.queryStringParameters.app);
    const options = {
      view: event.queryStringParameters.view
    };
    if (event.queryStringParameters.filterByFormula) {
      options.filterByFormula = event.queryStringParameters.filterByFormula;
    }
    const learningData = await base(table)
      .select(options)
      .all();

    const formattedLearningData = learningData.map(datum => ({
      id: datum.id,
      ...datum.fields
    }));
    return formattedReturn(200, { records: formattedLearningData });
  } catch (err) {
    console.error(err);
    return formattedReturn(500, { err });
  }
};

const outputRequest = async event => {
  const table = event.queryStringParameters.base;
  const view = event.queryStringParameters.view;
  if (
    view != "VKS_output" &&
    view != "SPR_output" &&
    view != "SentenceCompletion_output"
  ) {
    throw Error("You can't access users' table(please?)");
  }
  const options = {
    api_key: process.env.API_KEY,
    view
  };
  if (event.queryStringParameters.pageSize) {
    options.pageSize = event.queryStringParameters.pageSize;
  }
  if (event.queryStringParameters.offset) {
    options.offset = event.queryStringParameters.offset;
  }
  const learningData = await getJSON(
    `https://api.airtable.com/v0/${
      event.queryStringParameters.app
    }/${table}?${queryString.stringify(options)}`
  );

  learningData.records = learningData.records.map(datum => ({
    id: datum.id,
    ...datum.fields
  }));

  return formattedReturn(200, learningData);
};

const getJSON = url => {
  return new Promise((resolve, reject) => {
    https
      .get(url, resp => {
        let data = "";

        resp.on("data", chunk => {
          data += chunk;
        });

        resp.on("end", () => {
          resolve(JSON.parse(data));
        });
      })
      .on("error", err => {
        reject({ err });
      });
  });
};
