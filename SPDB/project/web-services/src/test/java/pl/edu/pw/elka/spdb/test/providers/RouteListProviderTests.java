package pl.edu.pw.elka.spdb.test.providers;

import junit.framework.TestCase;
import org.junit.Test;
import pl.edu.pw.elka.spdb.adapters.RouteListAdapter;
import pl.edu.pw.elka.spdb.coordinates.Coordinates;
import pl.edu.pw.elka.spdb.model.MapEntry;
import pl.edu.pw.elka.spdb.model.Route;
import pl.edu.pw.elka.spdb.providers.RouteListProvider;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.time.Duration;
import java.util.ArrayList;

public class RouteListProviderTests extends TestCase {
    @Test
    public void testWriteToMethod() {
        try (OutputStream outputStream = new ByteArrayOutputStream()) {
            RouteListProvider provider = new RouteListProvider();
            RouteListAdapter adapter = new RouteListAdapter(new ArrayList<>());
            MapEntry firstMapEntry = new MapEntry(new Coordinates(52.1234, 21.6789));
            firstMapEntry.setId(16L);
            MapEntry secondMapEntry = new MapEntry(new Coordinates(52.6789, 21.1234));
            secondMapEntry.setId(17L);
            Route routeForwards = firstMapEntry.addRoute(secondMapEntry, Duration.ofMinutes(5));
            Route routeBackwards = secondMapEntry.addRoute(firstMapEntry, Duration.ofMinutes(5));
            adapter.getRoutes().add(routeForwards);
            adapter.getRoutes().add(routeBackwards);

            provider.writeTo(adapter, null, null, null, null, null, outputStream);
            String routeAsJson = outputStream.toString();

            assertEquals("[{\"routeFrom\":{\"id\":16,\"wkt\":\"POINT( 52.12340000 21.67890000 )\"}," +
                            "\"routeTo\":{\"id\":17,\"wkt\":\"POINT( 52.67890000 21.12340000 )\"},\"duration\":300}," +
                            "{\"routeFrom\":{\"id\":17,\"wkt\":\"POINT( 52.67890000 21.12340000 )\"}," +
                            "\"routeTo\":{\"id\":16,\"wkt\":\"POINT( 52.12340000 21.67890000 )\"},\"duration\":300}]",
                            routeAsJson);
        } catch (Exception ex) {
            fail("Exception was thrown");
        }
    }
}
