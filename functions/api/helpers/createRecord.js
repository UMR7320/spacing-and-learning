const Airtable = require("airtable");

Airtable.configure({ apiKey: process.env.API_KEY });

const formattedReturn = require("./formattedReturn");
module.exports = async (event) => {
  const fields = JSON.parse(event.body);
  try {
    const base = Airtable.base(process.env.AIRTABLE_BASE);
    const createdRecord = await base(event.queryStringParameters.base).create(
      fields

    );
    let formattedRecord;
    if (Array.isArray(createdRecord)) {
      formattedRecord = createdRecord.map((datum) => ({
        id: datum.id,
        ...datum.fields,
      }));
    } else {
      formattedRecord = [{
        id: createdRecord.id,
        ...createdRecord.fields,
      }];
    }
    console.log(formattedRecord);
    return formattedReturn(200, formattedRecord.shift());
  } catch (err) {
    console.error(err);
    return formattedReturn(err.statusCode, { "error": err.error, "message": err.message });
  }
};
