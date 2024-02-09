const Airtable = require("airtable");
const { DateTime } = require("luxon");

Airtable.configure({ apiKey: process.env.API_KEY });

exports.handler = async event => {
  try {
    const base = Airtable.base(process.env.AIRTABLE_BASE);
    const code = event.queryStringParameters.code;
    const users = await base("users").select({
      filterByFormula: `{Connection_Code} = "${code}"`
    }).firstPage();

    if (users.length === 0) {
      return {
        statusCode: 200,
        body: JSON.stringify({
          success: false,
          reason: "Ce code n'est associé à aucun élève."
        }),
        headers: {
          "Content-Type": "application/json"
        },
      }
    }
    const user = users[0];
    const group = user.fields.Group;
    let now = DateTime.now().setZone("Europe/Paris");
    // let users override the current date for testing
    const date = event.queryStringParameters.date;
    if (date) {
      now = DateTime.fromISO(date).setZone("Europe/Paris");
    }
    console.log(now);

    const session = selectSession(now, group);
    if (!session) {
      return {
        statusCode: 200,
        body: JSON.stringify({
          success: false,
          reason: `Pas de session trouvée dans le calendrier du groupe ${group}.`
        }),
        headers: {
          "Content-Type": "application/json"
        },
      }
    }
    return {
      statusCode: 200,
      body: JSON.stringify({
        success: true,
        userId: user.id,
        session: session
      }),
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

const calendar = {
  "A-3": [
    { date: "2024-02-23", session: "pretest" },
    { date: "2024-03-18", session: "session1" },
    { date: "2024-03-21", session: "session2" },
    { date: "2024-03-25", session: "session3" },
    { date: "2024-04-08", session: "post-test-diff" },
  ],
  "A-7": [
    { date: "2024-02-23", session: "pretest" },
    { date: "2024-03-11", session: "session1" },
    { date: "2024-03-18", session: "session2" },
    { date: "2024-03-25", session: "session3" },
    { date: "2024-04-08", session: "post-test-diff" },
  ],
  "B-3": [
    { date: "2024-02-19", session: "pretest" },
    { date: "2024-03-18", session: "session1" },
    { date: "2024-03-21", session: "session2" },
    { date: "2024-03-25", session: "session3" },
    { date: "2024-04-08", session: "post-test-diff" },
  ],
  "B-7": [
    { date: "2024-02-19", session: "pretest" },
    { date: "2024-03-11", session: "session1" },
    { date: "2024-03-18", session: "session2" },
    { date: "2024-03-25", session: "session3" },
    { date: "2024-04-08", session: "post-test-diff" },
  ],
}

const selectSession = (now, group) => {
  match = calendar[group].find(session => {
    const date = DateTime.fromISO(session.date, { zone: "Europe/Paris" });
    console.log(date);
    return now.hasSame(date, 'day');
  });


  return match ? match.session : null;
}
