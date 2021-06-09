const Airtable = require("airtable");

Airtable.configure({
  endpointUrl: process.env.API_URL,
  apiKey: process.env.API_KEY,
});

const formattedReturn = require("./formattedReturn");
module.exports = async (event) => {
  const fields = JSON.parse(event.body);
  try {
    const base = Airtable.base(event.queryStringParameters.app);
    const createdRecord = await base(event.queryStringParameters.base).create(
      fields

    );
    const formattedRecord = createdRecord.map((datum) => ({
      id: datum.id,
      ...datum.fields,
    }));
    console.log(formattedRecord)
    return formattedReturn(200, createdRecord.shift());
  } catch (err) {
    console.error(err);
    return formattedReturn(err.statusCode, { "error": err.error, "message": err.message });
  }
};
