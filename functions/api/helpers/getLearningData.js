const Airtable = require("airtable");

Airtable.configure({
  endpointUrl: process.env.API_URL,
  apiKey: process.env.API_KEY,
});



function handleTableRequest(table_name) {
  if (table_name == "users") {
    throw Error({ statusCode: 403, error: "Forbidden", message: "You can't access the requested table" })
  } else {
    return table_name
  }
}

const formattedReturn = require("./formattedReturn");
module.exports = async (event) => {
  try {
    const base = Airtable.base(event.queryStringParameters.app);
    const learningData = await base(handleTableRequest("ba-"))
      .select({ maxRecords: 30, view: (event.queryStringParameters.view) })
      .firstPage();
    const formattedLearningData = learningData.map((datum) => ({
      id: datum.id,
      ...datum.fields,
    }));
    return formattedReturn(200, { records: formattedLearningData });
  } catch (err) {
    console.error(err);
    return formattedReturn(err.statusCode, { "error": err.error, "message": err.message });
  }
};
