package pl.edu.pw.elka.spdb.test.services;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.reflect.TypeToken;
import junit.framework.TestCase;
import org.apache.cxf.helpers.IOUtils;
import org.apache.cxf.jaxrs.client.WebClient;
import org.junit.Test;
import pl.edu.pw.elka.spdb.adapters.DurationTypeAdapter;
import pl.edu.pw.elka.spdb.model.MapEntry;
import pl.edu.pw.elka.spdb.model.Route;

import javax.ws.rs.core.Response;
import java.io.InputStream;
import java.time.Duration;
import java.util.List;

public class MapEntryServiceIT extends TestCase {
    private static String endpointUrl;

    @Override
    protected void setUp() throws Exception {
        super.setUp();
        endpointUrl = System.getProperty("service.url");
    }

    @Test
    public void testGetNearestMapEntry() throws Exception {
        WebClient client = WebClient.create(endpointUrl + "/entry/nearest/52.2206062/21.0105747");

        Response response = client.accept("application/json").get();
        String content = IOUtils.toString((InputStream) response.getEntity());
        MapEntry mapEntry = new Gson().fromJson(content, MapEntry.class);

        assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
        assertEquals(52.220067, mapEntry.getCoordinates().getLatitude());
        assertEquals(21.012119, mapEntry.getCoordinates().getLongitude());
    }

    @Test
    public void testGetShortestPath() throws Exception {
        Long startingId = getMapEntryId(52.220067, 21.012119);
        Long finishingId = getMapEntryId(52.230014, 21.011886);
        String relativeUrl = String.format("/entry/shortestPath/%d/%d", startingId, finishingId);
        WebClient client = WebClient.create(endpointUrl + relativeUrl);
        Response response = client.accept("application/json").get();
        String content = IOUtils.toString((InputStream) response.getEntity());
        Gson gson = new GsonBuilder().excludeFieldsWithoutExposeAnnotation().registerTypeAdapter(Duration.class,
                new DurationTypeAdapter()).create();
        List<Route> routes = gson.fromJson(content, new TypeToken<List<Route>>(){}.getType());

        assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
        assertEquals(4, routes.size());
        assertEquals(52.220067, routes.get(0).getRouteFrom().getCoordinates().getLatitude());
        assertEquals(21.012119, routes.get(0).getRouteFrom().getCoordinates().getLongitude());
        assertEquals(52.219893, routes.get(0).getRouteTo().getCoordinates().getLatitude());
        assertEquals(21.018152, routes.get(0).getRouteTo().getCoordinates().getLongitude());

        assertEquals(52.219893, routes.get(1).getRouteFrom().getCoordinates().getLatitude());
        assertEquals(21.018152, routes.get(1).getRouteFrom().getCoordinates().getLongitude());
        assertEquals(52.223232, routes.get(1).getRouteTo().getCoordinates().getLatitude());
        assertEquals(21.015984, routes.get(1).getRouteTo().getCoordinates().getLongitude());

        assertEquals(52.223232, routes.get(2).getRouteFrom().getCoordinates().getLatitude());
        assertEquals(21.015984, routes.get(2).getRouteFrom().getCoordinates().getLongitude());
        assertEquals(52.226229, routes.get(2).getRouteTo().getCoordinates().getLatitude());
        assertEquals(21.014161, routes.get(2).getRouteTo().getCoordinates().getLongitude());

        assertEquals(52.226229, routes.get(3).getRouteFrom().getCoordinates().getLatitude());
        assertEquals(21.014161, routes.get(3).getRouteFrom().getCoordinates().getLongitude());
        assertEquals(52.230014, routes.get(3).getRouteTo().getCoordinates().getLatitude());
        assertEquals(21.011886, routes.get(3).getRouteTo().getCoordinates().getLongitude());
    }

    private Long getMapEntryId(double latitude, double longitude) throws Exception {
        String relativeUrl = String.format("/entry/nearest/%f/%f", latitude, longitude).replace(",", ".");
        WebClient client = WebClient.create(endpointUrl + relativeUrl);
        Response response = client.accept("application/json").get();
        String content = IOUtils.toString((InputStream) response.getEntity());
        MapEntry entry = new Gson().fromJson(content, MapEntry.class);

        return entry.getId();
    }
}
