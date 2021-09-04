import sqlite3 from 'better-sqlite3';

export function getConnection() {
  return sqlite3('content.sqlite3', {});
}
