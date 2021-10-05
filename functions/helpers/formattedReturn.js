module.exports = (statusCode, body) => {
    return {
        statusCode,
        body: JSON.stringify(body),
        headers: {
          "Content-Type": "application/json"
        },
    };
};
