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
import pl.edu.pw.elka.spdb.coordinates.Coordinates;
import pl.edu.pw.elka.spdb.dao.entries.IMapEntryDAO;
import pl.edu.pw.elka.spdb.model.MapEntry;
import pl.edu.pw.elka.spdb.model.PublicTransportRoute;
import pl.edu.pw.elka.spdb.model.Route;

import java.time.Duration;
import java.util.List;

@ContextConfiguration(locations = "classpath:/spring/testContext.xml")
@RunWith(SpringJUnit4ClassRunner.class)
@Transactional
public class Neo4jMapEntryDAOTests extends TestCase {
    @Autowired
    private IMapEntryDAO mapEntryDAO;

    @Autowired
    private Neo4jTemplate template;

    @Rollback(false)
    @BeforeTransaction
    public void cleanUp() {
        Neo4jHelper.cleanDb(template);
    }

    @Test
    public void testInsertAndFindLocationMethods() {
        MapEntry universityOfTechnology = new MapEntry(new Coordinates(52.2206062, 21.0105747));

        Long mapEntryId = mapEntryDAO.insertMapEntry(universityOfTechnology).getId();
        MapEntry foundMapEntry = mapEntryDAO.findMapEntryById(mapEntryId);

        assertNotNull(foundMapEntry);
        assertEquals(universityOfTechnology, foundMapEntry);
    }

    @Test
    public void testAddRouteMethod() {
        MapEntry universityOfTechnology = new MapEntry(new Coordinates(52.2206062, 21.0105747));
        MapEntry subway = new MapEntry(new Coordinates(52.2190664, 21.0153627));
        MapEntry mlocinyUndergroundStation = new MapEntry(new Coordinates(52.290513, 20.930355));
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
        MapEntry universityOfTechnology = new MapEntry(new Coordinates(52.2206062, 21.0105747));
        MapEntry subway = new MapEntry(new Coordinates(52.2190664, 21.0153627));
        MapEntry mlocinyUndergroundStation = new MapEntry(new Coordinates(52.290513, 20.930355));
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
        MapEntry universityOfTechnology = new MapEntry(new Coordinates(52.2206062, 21.0105747));
        MapEntry saviourSquare = new MapEntry(new Coordinates(52.219929, 21.017988));
        MapEntry centralUndergroundStation = new MapEntry(new Coordinates(52.229896, 21.011701));
        MapEntry independenceStreet = new MapEntry(new Coordinates(52.220159, 21.005006));
        MapEntry goldenTerrace = new MapEntry(new Coordinates(52.227809, 21.001938));
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

        List<Route> fastestRoute = mapEntryDAO.findFastestRoute(universityOfTechnology, centralUndergroundStation);

        assertEquals(3, fastestRoute.size());
        assertEquals(universityOfTechnology.getId(), fastestRoute.get(0).getRouteFrom().getId());
        assertEquals(independenceStreet.getId(), fastestRoute.get(0).getRouteTo().getId());
        assertEquals(independenceStreet.getId(), fastestRoute.get(1).getRouteFrom().getId());
        assertEquals(goldenTerrace.getId(), fastestRoute.get(1).getRouteTo().getId());
        assertEquals(goldenTerrace.getId(), fastestRoute.get(2).getRouteFrom().getId());
        assertEquals(centralUndergroundStation.getId(), fastestRoute.get(2).getRouteTo().getId());
    }

    @Test
    public void testFindNearestMapEntryMethod() {
        MapEntry saviourSquare = new MapEntry(new Coordinates(52.219929, 21.017988));
        MapEntry subway = new MapEntry(new Coordinates(52.2190664, 21.0153627));
        MapEntry mlocinyUndergroundStation = new MapEntry(new Coordinates(52.290513, 20.930355));
        mapEntryDAO.insertMapEntry(saviourSquare);
        subway = mapEntryDAO.insertMapEntry(subway);
        mapEntryDAO.insertMapEntry(mlocinyUndergroundStation);

        MapEntry nearestToUniversityOfTechnology = mapEntryDAO.findNearestMapEntry(52.2206062, 21.0105747);

        assertNotNull(nearestToUniversityOfTechnology);
        assertEquals(subway.getId(), nearestToUniversityOfTechnology.getId());
    }

    @Test
    public void testFindRouteBetweenMethod() {
        MapEntry universityOfTechnology = new MapEntry(new Coordinates(52.2206062, 21.0105747));
        MapEntry saviourSquare = new MapEntry(new Coordinates(52.219929, 21.017988));
        universityOfTechnology = mapEntryDAO.insertMapEntry(universityOfTechnology);
        saviourSquare = mapEntryDAO.insertMapEntry(saviourSquare);
        Route universityToSaviourSquareRoute = universityOfTechnology.addRoute(saviourSquare, Duration.ofMinutes(3));
        universityToSaviourSquareRoute = template.save(universityToSaviourSquareRoute);

        Route foundFromUniversityToSaviourSquare = mapEntryDAO.findRouteBetween(universityOfTechnology, saviourSquare);
        Route foundFromSaviourSquareToUniversity = mapEntryDAO.findRouteBetween(saviourSquare, universityOfTechnology);
        Route foundFromUniversityToSaviourSquareById = mapEntryDAO.findRouteBetween(universityOfTechnology.getId(),
                saviourSquare.getId());
        Route foundFromSaviourSquareToUniversityById = mapEntryDAO.findRouteBetween(saviourSquare.getId(),
                universityOfTechnology.getId());

        assertNotNull(foundFromUniversityToSaviourSquare);
        assertEquals(universityToSaviourSquareRoute.getId(), foundFromUniversityToSaviourSquare.getId());
        assertEquals(universityToSaviourSquareRoute.getDuration(), foundFromUniversityToSaviourSquare.getDuration());
        assertEquals(universityOfTechnology.getWkt(), foundFromUniversityToSaviourSquare.getRouteFrom().getWkt());
        assertEquals(saviourSquare.getWkt(), foundFromUniversityToSaviourSquare.getRouteTo().getWkt());
        assertNull(foundFromSaviourSquareToUniversity);
        assertNotNull(foundFromUniversityToSaviourSquareById);
        assertEquals(universityToSaviourSquareRoute.getId(), foundFromUniversityToSaviourSquareById.getId());
        assertEquals(universityToSaviourSquareRoute.getDuration(),
                foundFromUniversityToSaviourSquareById.getDuration());
        assertEquals(universityOfTechnology.getWkt(), foundFromUniversityToSaviourSquareById.getRouteFrom().getWkt());
        assertEquals(saviourSquare.getWkt(), foundFromUniversityToSaviourSquareById.getRouteTo().getWkt());
        assertNull(foundFromSaviourSquareToUniversityById);
    }

    @Test
    public void testFindNearestPublicTransportStopMethod() {
        MapEntry saviourSquare = new MapEntry(new Coordinates(52.219929, 21.017988), true);
        MapEntry subway = new MapEntry(new Coordinates(52.2190664, 21.0153627), false);
        MapEntry mlocinyUndergroundStation = new MapEntry(new Coordinates(52.290513, 20.930355), true);
        saviourSquare = mapEntryDAO.insertMapEntry(saviourSquare);
        mapEntryDAO.insertMapEntry(subway);
        mapEntryDAO.insertMapEntry(mlocinyUndergroundStation);

        MapEntry nearestStopToUniversityOfTechnology =
                mapEntryDAO.findNearestPublicTransportStop(52.2206062, 21.0105747);

        assertNotNull(nearestStopToUniversityOfTechnology);
        assertEquals(saviourSquare.getId(), nearestStopToUniversityOfTechnology.getId());
    }

    @Test
    public void testFindPublicTransportRouteBetween() {
        MapEntry universitySquare = mapEntryDAO.insertMapEntry(new MapEntry(new Coordinates(52.2200113, 21.0120177),
                true));
        MapEntry saviourSquare = mapEntryDAO.insertMapEntry(new MapEntry(new Coordinates(52.219929, 21.017988), true));
        MapEntry constitutionSquare = mapEntryDAO.insertMapEntry(new MapEntry(new Coordinates(52.222285, 21.016180),
                true));
        PublicTransportRoute universityToSaviourSquareRoute = new PublicTransportRoute(15,
                universitySquare.addRoute(saviourSquare, Duration.ofMinutes(3)));
        template.save(universityToSaviourSquareRoute);

        PublicTransportRoute foundRouteFromUniversityToSaviourSquare = mapEntryDAO.findPublicTransportRouteBetween
                (universitySquare, saviourSquare);
        PublicTransportRoute foundRouteFromSaviourToUniversitySquare = mapEntryDAO.findPublicTransportRouteBetween
                (saviourSquare, universitySquare);
        PublicTransportRoute foundRouteFromConstitutionToUniversitySquare = mapEntryDAO
                .findPublicTransportRouteBetween(constitutionSquare, universitySquare);

        assertNotNull(foundRouteFromUniversityToSaviourSquare);
        assertEquals(universitySquare, foundRouteFromUniversityToSaviourSquare.getRouteFrom());
        assertEquals(saviourSquare, foundRouteFromUniversityToSaviourSquare.getRouteTo());
        assertEquals(Duration.ofMinutes(3), foundRouteFromUniversityToSaviourSquare.getDuration());
        assertEquals(15, foundRouteFromUniversityToSaviourSquare.getLine());
        assertNull(foundRouteFromSaviourToUniversitySquare);
        assertNull(foundRouteFromConstitutionToUniversitySquare);
    }
}
