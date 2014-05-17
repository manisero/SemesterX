package pl.edu.pw.elka.spdb.tests.population;

import junit.framework.TestCase;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.neo4j.graphdb.Transaction;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.neo4j.support.Neo4jTemplate;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import pl.edu.pw.elka.spdb.model.MapEntry;
import pl.edu.pw.elka.spdb.model.PublicTransportRoute;
import pl.edu.pw.elka.spdb.model.Route;
import pl.edu.pw.elka.spdb.population.IDataPopulator;

@ContextConfiguration(locations = "classpath:/spring/context.xml")
@RunWith(SpringJUnit4ClassRunner.class)
public class DataPopulatorTests extends TestCase {
    @Autowired
    private Neo4jTemplate template;

    @Autowired
    private IDataPopulator dataPopulator;

    @Test
    public void testPopulateMethod() {
        dataPopulator.populate();

        Transaction transaction = template.getGraphDatabaseService().beginTx();
        long entriesCount = template.count(MapEntry.class);
        long routesCount = template.count(Route.class);
        long publicTransportRoutesCount = template.count(PublicTransportRoute.class);
        transaction.success();

        assertTrue(entriesCount > 0);
        assertTrue(routesCount > 0);
        assertTrue(publicTransportRoutesCount > 0);
    }
}
