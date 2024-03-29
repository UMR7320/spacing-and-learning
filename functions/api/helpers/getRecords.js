const Airtable = require("airtable");
const queryString = require("querystring");
const getJson = require("./getJson");
const formattedReturn = require("./formattedReturn");

Airtable.configure({ apiKey: process.env.API_KEY });

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
    const base = Airtable.base(process.env.AIRTABLE_BASE);
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
    token: process.env.API_KEY,
    view
  };
  if (event.queryStringParameters.pageSize) {
    options.pageSize = event.queryStringParameters.pageSize;
  }
  if (event.queryStringParameters.offset) {
    options.offset = event.queryStringParameters.offset;
  }
  const learningData = await getJson(
    `https://api.airtable.com/v0/${event.queryStringParameters.app}/${table}?${queryString.stringify(options)}`,
    { headers: { Authorization: `Bearer ${process.env.API_KEY}` } }
  );

  learningData.records = learningData.records.map(datum => ({
    id: datum.id,
    ...datum.fields
  }));

  return formattedReturn(200, learningData);
};
