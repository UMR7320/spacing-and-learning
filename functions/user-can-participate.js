const Airtable = require("airtable");
const getJson = require("./api/helpers/getJson");
const getRecords = require("./api/helpers/getRecords");

Airtable.configure({
  endpointUrl: process.env.API_URL,
  apiKey: process.env.API_KEY
});

exports.handler = async (event, context) => {
  try {
    const base = Airtable.base(process.env.AIRTABLE_BASE);
    const userId = event.queryStringParameters.id;
    const currentUser = await base("users").find(userId);
    const users = await base("session1_users")
      .select()
      .all()

    const currentUserEmail = currentUser.get("Email");
    const userEmails = users.map(record => record.get('email'));

    return { statusCode: 200, body: JSON.stringify(!userEmails.includes(currentUserEmail)) };
  } catch (error) {
    console.log(error);
    return {
      statusCode: 500,
      body: JSON.stringify({ error: 'Failed fetching data' }),
    };
  }
};
