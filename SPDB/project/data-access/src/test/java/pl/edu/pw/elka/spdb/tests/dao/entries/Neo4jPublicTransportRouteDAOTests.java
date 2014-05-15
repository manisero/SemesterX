package pl.edu.pw.elka.spdb.tests.dao.entries;

import junit.framework.TestCase;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Relationship;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.neo4j.support.Neo4jTemplate;
import org.springframework.data.neo4j.support.node.Neo4jHelper;
import org.springframework.test.annotation.Rollback;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.transaction.BeforeTransaction;
import org.springframework.transaction.annotation.Transactional;
import pl.edu.pw.elka.spdb.coordinates.Coordinates;
import pl.edu.pw.elka.spdb.dao.entry.IMapEntryDAO;
import pl.edu.pw.elka.spdb.dao.publictransportroute.IPublicTransportRouteDAO;
import pl.edu.pw.elka.spdb.model.MapEntry;
import pl.edu.pw.elka.spdb.model.PublicTransportRoute;
import pl.edu.pw.elka.spdb.model.Route;

import java.time.Duration;
import java.util.List;

@ContextConfiguration(locations = "classpath:/spring/testContext.xml")
@RunWith(SpringJUnit4ClassRunner.class)
@Transactional
public class Neo4jPublicTransportRouteDAOTests extends TestCase {
    @Autowired
    private IMapEntryDAO mapEntryDAO;

    @Autowired
    private IPublicTransportRouteDAO publicTransportRouteDAO;

    @Autowired
    private Neo4jTemplate template;

    @Rollback(false)
    @BeforeTransaction
    public void cleanUp() {
        Neo4jHelper.cleanDb(template);
    }

    @Test
    public void testFindPublicTransportRouteBetweenMethod() {
        MapEntry universitySquare = mapEntryDAO.insertMapEntry(new MapEntry(new Coordinates(52.2200113, 21.0120177),
                true));
        MapEntry saviourSquare = mapEntryDAO.insertMapEntry(new MapEntry(new Coordinates(52.219929, 21.017988), true));
        MapEntry constitutionSquare = mapEntryDAO.insertMapEntry(new MapEntry(new Coordinates(52.222285, 21.016180),
                true));
        PublicTransportRoute universityToSaviourSquareRoute = new PublicTransportRoute(15,
                universitySquare.addRoute(saviourSquare, Duration.ofMinutes(3)));
        template.save(universityToSaviourSquareRoute);

        PublicTransportRoute foundRouteFromUniversityToSaviourSquare =
                publicTransportRouteDAO.findPublicTransportRouteBetween(universitySquare, saviourSquare);
        PublicTransportRoute foundRouteFromSaviourToUniversitySquare =
                publicTransportRouteDAO.findPublicTransportRouteBetween(saviourSquare, universitySquare);
        PublicTransportRoute foundRouteFromConstitutionToUniversitySquare =
                publicTransportRouteDAO.findPublicTransportRouteBetween(constitutionSquare, universitySquare);

        assertNotNull(foundRouteFromUniversityToSaviourSquare);
        assertEquals(universitySquare, foundRouteFromUniversityToSaviourSquare.getRouteFrom());
        assertEquals(saviourSquare, foundRouteFromUniversityToSaviourSquare.getRouteTo());
        assertEquals(Duration.ofMinutes(3), foundRouteFromUniversityToSaviourSquare.getDuration());
        assertEquals(15, foundRouteFromUniversityToSaviourSquare.getLine());
        assertNull(foundRouteFromSaviourToUniversitySquare);
        assertNull(foundRouteFromConstitutionToUniversitySquare);
    }

    @Test
    public void testFindPublicTransportRouteBetweenByIdMethod() {
        MapEntry universitySquare = mapEntryDAO.insertMapEntry(new MapEntry(new Coordinates(52.2200113, 21.0120177),
                true));
        MapEntry saviourSquare = mapEntryDAO.insertMapEntry(new MapEntry(new Coordinates(52.219929, 21.017988), true));
        MapEntry constitutionSquare = mapEntryDAO.insertMapEntry(new MapEntry(new Coordinates(52.222285, 21.016180),
                true));
        PublicTransportRoute universityToSaviourSquareRoute = new PublicTransportRoute(15,
                universitySquare.addRoute(saviourSquare, Duration.ofMinutes(3)));
        template.save(universityToSaviourSquareRoute);

        PublicTransportRoute foundRouteFromUniversityToSaviourSquare =
                publicTransportRouteDAO.findPublicTransportRouteBetween(universitySquare.getId(),
                        saviourSquare.getId());
        PublicTransportRoute foundRouteFromSaviourToUniversitySquare =
                publicTransportRouteDAO.findPublicTransportRouteBetween(saviourSquare.getId(),
                        universitySquare.getId());
        PublicTransportRoute foundRouteFromConstitutionToUniversitySquare =
                publicTransportRouteDAO.findPublicTransportRouteBetween(constitutionSquare.getId(),
                        universitySquare.getId());

        assertNotNull(foundRouteFromUniversityToSaviourSquare);
        assertEquals(universitySquare, foundRouteFromUniversityToSaviourSquare.getRouteFrom());
        assertEquals(saviourSquare, foundRouteFromUniversityToSaviourSquare.getRouteTo());
        assertEquals(Duration.ofMinutes(3), foundRouteFromUniversityToSaviourSquare.getDuration());
        assertEquals(15, foundRouteFromUniversityToSaviourSquare.getLine());
        assertNull(foundRouteFromSaviourToUniversitySquare);
        assertNull(foundRouteFromConstitutionToUniversitySquare);
    }

    @Test
    public void testFindFastestPublicTransportRoute() {
        MapEntry universitySquare = mapEntryDAO.insertMapEntry(new MapEntry(new Coordinates(52.2200113, 21.0120177),
                true));
        MapEntry universityUndergroundStation = mapEntryDAO.insertMapEntry(new MapEntry(new Coordinates(52.219932,
                21.015511), true));
        MapEntry saviourSquare = mapEntryDAO.insertMapEntry(new MapEntry(new Coordinates(52.219929, 21.017988), true));
        MapEntry constitutionSquare = mapEntryDAO.insertMapEntry(new MapEntry(new Coordinates(52.222285, 21.016180),
                true));
        Route universitySquareToUnderground = template.save(universitySquare.addRoute(universityUndergroundStation,
                Duration.ofMinutes(2)));
        Route universityUndergroundToSaviourSquare = template.save(universityUndergroundStation.addRoute
                (saviourSquare, Duration.ofMinutes(3)));
        Route universityUndergroundToConstitutionSquare = template.save(universityUndergroundStation.addRoute
                (constitutionSquare, Duration.ofMinutes(5)));
        template.save(new PublicTransportRoute(15, universitySquareToUnderground));
        template.save(new PublicTransportRoute(15, universityUndergroundToSaviourSquare));
        template.save(new PublicTransportRoute(502, universityUndergroundToConstitutionSquare));

        List<PublicTransportRoute> routeFromUniversitySquareToSaviourSquare = publicTransportRouteDAO
                .findFastestPublicTransportRoute(universitySquare, saviourSquare, Duration.ofMinutes(5));
        List<PublicTransportRoute> routeFromUniversitySquareToConstitutionSquare = publicTransportRouteDAO
                .findFastestPublicTransportRoute(universitySquare, constitutionSquare, Duration.ofMinutes(5));

        assertEquals(2, routeFromUniversitySquareToSaviourSquare.size());
        assertEquals(15, routeFromUniversitySquareToSaviourSquare.get(0).getLine());

        assertEquals(2, routeFromUniversitySquareToConstitutionSquare.size());

        Double undergroundToSaviourSquareCost = evaluator.getCost
                (fifteenFromUniversityUndergroundToSaviourSquareRelationship, Direction.OUTGOING);
        Double undergroundToConstitutionSquareCost = evaluator.getCost
                (fiveHundredAndTwoFromUniversityUndergroundToConstitutionSquareRelationship, Direction.OUTGOING);

        assertEquals(180.0, undergroundToSaviourSquareCost);
        assertEquals(900.0, undergroundToConstitutionSquareCost);
    }
}
