package pl.edu.pw.elka.spdb.tests.database;

import junit.framework.TestCase;
import org.junit.Test;
import org.neo4j.graphdb.GraphDatabaseService;
import pl.edu.pw.elka.spdb.database.IDatabaseFactory;
import pl.edu.pw.elka.spdb.database.impl.DatabaseFactory;

public class DatabaseFactoryTests extends TestCase {
    @Test
    public void createDatabaseTest() {
        IDatabaseFactory databaseFactory = new DatabaseFactory();

        GraphDatabaseService service = databaseFactory.createDatabase("target/test.db");
        boolean serviceIsOpen = service != null;
        service.shutdown();

        assertTrue(serviceIsOpen);
    }
}
