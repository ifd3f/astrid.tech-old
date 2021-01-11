function getEnv(key, devDefault) {
  if (process.env[key]) {
    return process.env[key];
  } else if (process.env.NODE_ENV == "production") {
    throw new Error(`We are in production, but ${key} was not specified!`);
  } else {
    console.warn(`No ${key} specified, defaulting to ${devDefault}`);
    return devDefault;
  }
}

module.exports = { getEnv };
