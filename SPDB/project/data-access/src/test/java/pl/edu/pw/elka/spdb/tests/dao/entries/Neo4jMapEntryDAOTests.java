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

import java.time.Duration;
import java.util.List;

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
        MapEntry mlocinyUndergroundStation = new MapEntry(52.290513, 20.930355);
        universityOfTechnology = mapEntryDAO.insertMapEntry(universityOfTechnology);
        subway = mapEntryDAO.insertMapEntry(subway);
        mlocinyUndergroundStation = mapEntryDAO.insertMapEntry(mlocinyUndergroundStation);
        template.save(universityOfTechnology.addRoute(subway, Duration.ofMinutes(5)));
        template.save(subway.addRoute(universityOfTechnology, Duration.ofMinutes(5)));

        universityOfTechnology = mapEntryDAO.findMapEntryById(universityOfTechnology.getId());
        subway = mapEntryDAO.findMapEntryById(subway.getId());
        mlocinyUndergroundStation = mapEntryDAO.findMapEntryById(mlocinyUndergroundStation.getId());

        assertTrue(universityOfTechnology.routesTo(subway));
        assertTrue(subway.routesTo(universityOfTechnology));
        assertFalse(universityOfTechnology.routesTo(mlocinyUndergroundStation));
        assertFalse(mlocinyUndergroundStation.routesTo(universityOfTechnology));
        assertFalse(subway.routesTo(mlocinyUndergroundStation));
        assertFalse(mlocinyUndergroundStation.routesTo(subway));
    }

    @Test
    public void testGetTravelTimeMethod() {
        MapEntry universityOfTechnology = new MapEntry(52.2206062, 21.0105747);
        MapEntry subway = new MapEntry(52.2190664, 21.0153627);
        MapEntry mlocinyUndergroundStation = new MapEntry(52.290513, 20.930355);
        universityOfTechnology = mapEntryDAO.insertMapEntry(universityOfTechnology);
        subway = mapEntryDAO.insertMapEntry(subway);
        mlocinyUndergroundStation = mapEntryDAO.insertMapEntry(mlocinyUndergroundStation);
        template.save(universityOfTechnology.addRoute(subway, Duration.ofMinutes(5)));

        universityOfTechnology = mapEntryDAO.findMapEntryById(universityOfTechnology.getId());
        subway = mapEntryDAO.findMapEntryById(subway.getId());
        mlocinyUndergroundStation = mapEntryDAO.findMapEntryById(mlocinyUndergroundStation.getId());

        assertTrue(universityOfTechnology.getTravelTime(subway).equals(Duration.ofMinutes(5)));
        assertNull(universityOfTechnology.getTravelTime(mlocinyUndergroundStation));
    }

    @Test
    public void testFindFastestRouteMethod() {
        MapEntry universityOfTechnology = new MapEntry(52.2206062, 21.0105747);
        MapEntry saviourSquare = new MapEntry(52.219929, 21.017988);
        MapEntry centralUndergroundStation = new MapEntry(52.229896, 21.011701);
        MapEntry independenceStreet = new MapEntry(52.220159, 21.005006);
        MapEntry goldenTerrace = new MapEntry(52.227809, 21.001938);
        universityOfTechnology = mapEntryDAO.insertMapEntry(universityOfTechnology);
        saviourSquare = mapEntryDAO.insertMapEntry(saviourSquare);
        centralUndergroundStation = mapEntryDAO.insertMapEntry(centralUndergroundStation);
        independenceStreet = mapEntryDAO.insertMapEntry(independenceStreet);
        goldenTerrace = mapEntryDAO.insertMapEntry(goldenTerrace);
        template.save(universityOfTechnology.addRoute(saviourSquare, Duration.ofMinutes(5)));
        template.save(saviourSquare.addRoute(centralUndergroundStation, Duration.ofMinutes(10)));
        template.save(universityOfTechnology.addRoute(independenceStreet, Duration.ofMinutes(3)));
        template.save(independenceStreet.addRoute(goldenTerrace, Duration.ofMinutes(5)));
        template.save(goldenTerrace.addRoute(centralUndergroundStation, Duration.ofMinutes(3)));

        List<MapEntry> fastestRoute = mapEntryDAO.findFastestRoute(universityOfTechnology, centralUndergroundStation);

        assertEquals(4, fastestRoute.size());
        assertEquals(universityOfTechnology.getId(), fastestRoute.get(0).getId());
        assertEquals(independenceStreet.getId(), fastestRoute.get(1).getId());
        assertEquals(goldenTerrace.getId(), fastestRoute.get(2).getId());
        assertEquals(centralUndergroundStation.getId(), fastestRoute.get(3).getId());
    }

    @Test
    public void testFindNearestMapEntryMethod() {
        MapEntry saviourSquare = new MapEntry(52.219929, 21.017988);
        MapEntry subway = new MapEntry(52.2190664, 21.0153627);
        MapEntry mlocinyUndergroundStation = new MapEntry(52.290513, 20.930355);
        mapEntryDAO.insertMapEntry(saviourSquare);
        subway = mapEntryDAO.insertMapEntry(subway);
        mapEntryDAO.insertMapEntry(mlocinyUndergroundStation);

        MapEntry nearestToUniversityOfTechnology = mapEntryDAO.findNearestMapEntry(52.2206062, 21.0105747);

        assertNotNull(nearestToUniversityOfTechnology);
        assertEquals(subway.getId(), nearestToUniversityOfTechnology.getId());
    }
}
