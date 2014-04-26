package pl.edu.pw.elka.spdb.database;

import org.neo4j.graphdb.GraphDatabaseService;

public interface IDatabaseFactory {
    GraphDatabaseService createDatabase();
    GraphDatabaseService createDatabase(String databasePath);
}
