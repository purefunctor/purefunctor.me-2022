-- Repository Table
CREATE TABLE IF NOT EXISTS repos (
  name  VARCHAR NOT NULL,
  owner VARCHAR NOT NULL,
  url   VARCHAR NOT NULL,
  lang  VARCHAR NOT NULL,
  desc  VARCHAR NOT NULL,
  comm  INTEGER NOT NULL,
  star  INTEGER NOT NULL,
  PRIMARY KEY ( name )
);

-- Blog Post Table
CREATE TABLE IF NOT EXISTS posts (
  title VARCHAR   NOT NULL,
  slug  VARCHAR   NOT NULL,
  cont  VARCHAR   NOT NULL,
  publ  TIMESTAMP NOT NULL,
  updt  TIMESTAMP NOT NULL,
  PRIMARY KEY ( slug )
);
