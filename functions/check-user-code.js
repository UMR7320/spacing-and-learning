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
    const calendarRecords = await base("calendar").select().all()
    const calendar = calendarRecords.map(({ fields }) => ({
      group: fields.Group_Name[0],
      session: fields.Session_Name[0].toLowerCase(),
      date: fields.Date,
    }));

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
    const group = user.fields.Group_Name[0];
    let now = DateTime.now().setZone("Europe/Paris");
    // let users override the current date for testing
    const date = event.queryStringParameters.date;
    if (date) {
      now = DateTime.fromISO(date).setZone("Europe/Paris");
    }

    const session = selectSession(calendar, now, group);
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

const selectSession = (calendar, now, group) => {
  match = calendar.find(session => {
    console.log(session, group);
    const date = DateTime.fromISO(session.date, { zone: "Europe/Paris" });
    return now.hasSame(date, 'day') && session.group === group;
  });

  return match ? match.session : null;
}
