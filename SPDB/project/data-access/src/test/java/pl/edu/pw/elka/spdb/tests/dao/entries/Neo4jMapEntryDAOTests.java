package pl.edu.pw.elka.spdb.tests.dao.entries;

import junit.framework.TestCase;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.neo4j.support.Neo4jTemplate;
import org.springframework.data.neo4j.support.node.Neo4jHelper;
import org.springframework.test.annotation.Rollback;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.transaction.BeforeTransaction;
import org.springframework.transaction.annotation.Transactional;
import pl.edu.pw.elka.spdb.dao.entries.IMapEntryDAO;
import pl.edu.pw.elka.spdb.model.MapEntry;

@ContextConfiguration(locations = "classpath:/spring/testContext.xml")
@RunWith(SpringJUnit4ClassRunner.class)
@Transactional
public class Neo4jMapEntryDAOTests extends TestCase {
    @Autowired private IMapEntryDAO mapEntryDAO;
    @Autowired private Neo4jTemplate template;

    @Rollback(false)
    @BeforeTransaction
    public void cleanUp() {
        Neo4jHelper.cleanDb(template);
    }

    @Test
    public void testInsertAndFindLocationMethods() {
        MapEntry universityOfTechnology = new MapEntry(52.2206062, 21.0105747);

        Long mapEntryId = mapEntryDAO.insertMapEntry(universityOfTechnology).getId();
        MapEntry foundMapEntry = mapEntryDAO.findMapEntryById(mapEntryId);

        assertNotNull(foundMapEntry);
        assertEquals(universityOfTechnology, foundMapEntry);
    }

    @Test
    public void testAddRouteMethod() {
        MapEntry universityOfTechnology = new MapEntry(52.2206062, 21.0105747);
        MapEntry subway = new MapEntry(52.2190664, 21.0153627);
        MapEntry mlocinyMetroStation = new MapEntry(52.290513, 20.930355);
        universityOfTechnology = mapEntryDAO.insertMapEntry(universityOfTechnology);
        subway = mapEntryDAO.insertMapEntry(subway);
        mlocinyMetroStation = mapEntryDAO.insertMapEntry(mlocinyMetroStation);
        template.save(universityOfTechnology.addRoute(subway));
        template.save(subway.addRoute(universityOfTechnology));

        universityOfTechnology = mapEntryDAO.findMapEntryById(universityOfTechnology.getId());
        subway = mapEntryDAO.findMapEntryById(subway.getId());
        mlocinyMetroStation = mapEntryDAO.findMapEntryById(mlocinyMetroStation.getId());

        assertTrue(universityOfTechnology.routesTo(subway));
        assertTrue(subway.routesTo(universityOfTechnology));
        assertFalse(universityOfTechnology.routesTo(mlocinyMetroStation));
        assertFalse(mlocinyMetroStation.routesTo(universityOfTechnology));
        assertFalse(subway.routesTo(mlocinyMetroStation));
        assertFalse(mlocinyMetroStation.routesTo(subway));
    }
}
