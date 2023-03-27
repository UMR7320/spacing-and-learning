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
    const currentUserProficiency = currentUser.get("proficiency");
    const userEmails = users.map(record => record.get('email'));

    if (userEmails.includes(currentUserEmail)) {
      return {
        statusCode: 200,
        body: JSON.stringify({
          userCanParticipate: false,
          reason: "You cannot participate multiple times to this experiment."
        }),
        headers: {
          "Content-Type": "application/json"
        },
      }
    }
    if (currentUserProficiency == "C2 (near-native)") {
      return {
        statusCode: 200,
        body: JSON.stringify({
          userCanParticipate: false,
          reason: "Native speakers cannot participate in this experiment."
        }),
        headers: {
          "Content-Type": "application/json"
        },
      }
    }
    return {
      statusCode: 200,
      body: JSON.stringify({ userCanParticipate: true }),
      headers: {
        "Content-Type": "application/json"
      },
    };
  } catch (error) {
    console.log(error);
    return {
      statusCode: 500,
      body: JSON.stringify({ error: 'Failed fetching data' }),
      headers: {
        "Content-Type": "application/json"
      },
    };
  }
};
