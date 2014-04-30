package pl.edu.pw.elka.spdb.test.providers;

import junit.framework.TestCase;
import org.junit.Test;
import pl.edu.pw.elka.spdb.coordinates.Coordinates;
import pl.edu.pw.elka.spdb.model.MapEntry;
import pl.edu.pw.elka.spdb.providers.MapEntryProvider;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.time.Duration;

public class MapEntryProviderTests extends TestCase {
    @Test
    public void testWriteToMethod() {
        try (OutputStream outputStream = new ByteArrayOutputStream()) {
            MapEntryProvider provider = new MapEntryProvider();
            MapEntry mapEntry = new MapEntry(new Coordinates(52.1234, 21.6789));
            mapEntry.setId(16L);
            mapEntry.addRoute(mapEntry, Duration.ofMinutes(5));

            provider.writeTo(mapEntry, null, null, null, null, null, outputStream);
            String mapEntryAsJson = outputStream.toString();

            assertEquals("{\"id\":16,\"wkt\":\"POINT( 52.12340000 21.67890000 )\"}", mapEntryAsJson);
        } catch (Exception ex) {
            fail("Exception was thrown");
        }
    }
}
