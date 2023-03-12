const Airtable = require("airtable");

Airtable.configure({
  endpointUrl: process.env.API_URL,
  apiKey: process.env.API_KEY,
});



function handleTableRequest(table_name) {
  if (table_name == "users") {
    throw Error("You can't access users' table(please?)")
  } else {
    return table_name
  }
}

const formattedReturn = require("./formattedReturn");
module.exports = async (event) => {
  try {
    const base = Airtable.base(process.env.AIRTABLE_BASE);
    const learningData = await base(handleTableRequest(event.queryStringParameters.base))
      .select({ maxRecords: 200, view: (event.queryStringParameters.view) })
      .all();

    const formattedLearningData = learningData.map((datum) => ({
      id: datum.id,
      ...datum.fields,
    }));
    return formattedReturn(200, { records: formattedLearningData });
  } catch (err) {
    console.error(err);
    return formattedReturn(500, { err });
  }
};
