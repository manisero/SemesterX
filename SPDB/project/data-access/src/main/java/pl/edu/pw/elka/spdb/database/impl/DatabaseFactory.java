package pl.edu.pw.elka.spdb.database.impl;

import com.google.inject.Inject;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.factory.GraphDatabaseFactory;
import pl.edu.pw.elka.spdb.configuration.IConfigurationProvider;
import pl.edu.pw.elka.spdb.database.IDatabaseFactory;

public class DatabaseFactory implements IDatabaseFactory {
    private final IConfigurationProvider configurationProvider;

    @Inject
    public DatabaseFactory(IConfigurationProvider configurationProvider) {
        this.configurationProvider = configurationProvider;
    }

    @Override
    public GraphDatabaseService createDatabase() {
        return createDatabase(configurationProvider.getDatabasePath());
    }

    @Override
    public GraphDatabaseService createDatabase(String databasePath) {
        return new GraphDatabaseFactory().newEmbeddedDatabase(databasePath);
    }
}
