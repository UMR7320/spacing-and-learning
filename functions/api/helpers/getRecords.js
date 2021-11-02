const Airtable = require("airtable");

Airtable.configure({
  endpointUrl: process.env.API_URL,
  apiKey: process.env.API_KEY
});

const formattedReturn = require("./formattedReturn");
module.exports = async event => {
  try {
    const table = event.queryStringParameters.base;
    const view = event.queryStringParameters.view;
    if (table == "users" && view != "VKS_output" && view != "SPR_output") {
      throw Error("You can't access users' table(please?)");
    }
    const base = Airtable.base(event.queryStringParameters.app);
    const options = {
      maxRecords: 200,
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
