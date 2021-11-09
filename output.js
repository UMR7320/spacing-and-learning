const formatVKSdata = records => {
  return records.flatMap(record => {
    return [
      "VKS_preTest",
      "VKS_postTest",
      "VKS_postTestDiff",
      "VKS_surprisePostTest"
    ].flatMap(session => {
      const answers = JSON.parse(record[session] || "[]");
      answers.forEach(answer => {
        answer.session = session.replace("VKS_", "");
        answer.userUID = record.UID;
        answer.condition = record.Group;
        answer.proficiency = record.YesNo;
      });
      return answers;
    });
  });
};

const formatSPRdata = records => {
  return records.flatMap(record => {
    return [
      "SPR_preTest",
      "SPR_postTest",
      "SPR_postTestDiff",
      "SPR_surprisePostTest"
    ].flatMap(session => {
      const answers = JSON.parse(record[session] || "[]");
      answers.forEach(answer => {
        answer.session = session.replace("SPR_", "");
        answer.userUID = record.UID;
        answer.condition = record.Group;
        answer.proficiency = record.YesNo;
      });
      return answers;
    });
  });
};

const formatSentenceCompletiondata = records => {
  return records.flatMap(record => {
    return [
      "SentenceCompletion_preTest",
      "SentenceCompletion_postTest",
      "SentenceCompletion_postTestDiff",
      "SentenceCompletion_surprisePostTest"
    ].flatMap(session => {
      const answers = JSON.parse(record[session] || "[]");
      answers.forEach(answer => {
        answer.session = session.replace("SentenceCompletion_", "");
        answer.userUID = record.UID;
        answer.condition = record.Group;
        answer.proficiency = record.YesNo;
      });
      return answers;
    });
  });
};

const displayAsTable = elementId => records => {
  let html;
  if (records.length === 0) {
    html = "No data available";
  } else {
    const keys = Object.keys(records[0]);
    html = "<table><thead><tr>";
    for (key of keys) {
      html += `<td>${key}</td>`;
    }
    html += "</tr></thead><tbody>";
    for (record of records) {
      html += "<tr>";
      for (key of keys) {
        html += `<td>${record[key]}</td>`;
      }
      html += "</tr>";
    }
    html += "</tbody><table>";
  }

  document.getElementById(elementId).insertAdjacentHTML("afterbegin", html);
};

fetch(
  "/.netlify/functions/api?app=appvKOc8FH0j48Hw1&base=users&view=VKS_output"
)
  .then(response => response.json())
  .then(json => json.records)
  .then(formatVKSdata)
  .then(displayAsTable("vks"));

fetch(
  "/.netlify/functions/api?app=appvKOc8FH0j48Hw1&base=users&view=SPR_output"
)
  .then(response => response.json())
  .then(json => json.records)
  .then(formatSPRdata)
  .then(displayAsTable("spr"));

fetch(
  "/.netlify/functions/api?app=appvKOc8FH0j48Hw1&base=users&view=SentenceCompletion_output"
)
  .then(response => response.json())
  .then(json => json.records)
  .then(formatSentenceCompletiondata)
  .then(displayAsTable("sentence-completion"));
