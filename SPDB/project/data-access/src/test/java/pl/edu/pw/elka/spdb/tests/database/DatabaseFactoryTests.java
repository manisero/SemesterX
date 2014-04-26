package pl.edu.pw.elka.spdb.tests.database;

import junit.framework.TestCase;
import org.junit.Test;
import org.neo4j.graphdb.GraphDatabaseService;
import pl.edu.pw.elka.spdb.configuration.IConfigurationProvider;
import pl.edu.pw.elka.spdb.configuration.impl.ConfigurationProvider;
import pl.edu.pw.elka.spdb.database.IDatabaseFactory;
import pl.edu.pw.elka.spdb.database.impl.DatabaseFactory;

import java.io.File;

public class DatabaseFactoryTests extends TestCase {
    @Test
    public void testCreateDatabaseMethod() {
        IConfigurationProvider configurationProvider = new ConfigurationProvider();
        IDatabaseFactory databaseFactory = new DatabaseFactory(configurationProvider);

        GraphDatabaseService service = databaseFactory.createDatabase("target/test.db");
        boolean serviceIsOpen = service != null;
        service.shutdown();

        File file = new File("target/test.db");

        if (file.exists()) {
            file.delete();
        }

        assertTrue(serviceIsOpen);
    }
}
