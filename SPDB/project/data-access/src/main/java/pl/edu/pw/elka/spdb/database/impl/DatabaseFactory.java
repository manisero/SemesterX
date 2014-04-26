package pl.edu.pw.elka.spdb.database.impl;

import org.neo4j.graphdb.GraphDatabaseService;
import pl.edu.pw.elka.spdb.database.IDatabaseFactory;

public class DatabaseFactory implements IDatabaseFactory {
    @Override
    public GraphDatabaseService createDatabase() {
        return null;
    }

    @Override
    public GraphDatabaseService createDatabase(String databasePath) {
        return null;
    }
}
