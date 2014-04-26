package pl.edu.pw.elka.spdb.tests;

import junit.framework.TestCase;
import org.junit.Test;
import pl.edu.pw.elka.spdb.dao.location.ILocationDAO;
import pl.edu.pw.elka.spdb.dao.location.impl.Neo4jLocationDAO;
import pl.edu.pw.elka.spdb.model.Coordinates;
import pl.edu.pw.elka.spdb.model.Location;

public class Neo4jLocationDAOTests extends TestCase {
    @Test
    public void testInsertAndFindLocationMethods() {
        ILocationDAO locationDAO = new Neo4jLocationDAO();
        Location location = new Location();
        location.setCoordinates(new Coordinates(52.2206062, 21.0105747));

        locationDAO.insertLocation(location);
        Location foundLocation = locationDAO.findLocation(location);

        assertNotNull(foundLocation);
        assertEquals(location, foundLocation);
    }
}
