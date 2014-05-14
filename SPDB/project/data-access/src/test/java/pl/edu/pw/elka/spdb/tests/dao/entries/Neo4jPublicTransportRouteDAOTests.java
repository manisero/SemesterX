package pl.edu.pw.elka.spdb.tests.dao.entries;

import junit.framework.TestCase;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.neo4j.support.Neo4jTemplate;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;
import pl.edu.pw.elka.spdb.coordinates.Coordinates;
import pl.edu.pw.elka.spdb.dao.entry.IMapEntryDAO;
import pl.edu.pw.elka.spdb.dao.publictransportroute.IPublicTransportRouteDAO;
import pl.edu.pw.elka.spdb.dao.route.IRouteDAO;
import pl.edu.pw.elka.spdb.model.MapEntry;
import pl.edu.pw.elka.spdb.model.PublicTransportRoute;

import java.time.Duration;

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
}
