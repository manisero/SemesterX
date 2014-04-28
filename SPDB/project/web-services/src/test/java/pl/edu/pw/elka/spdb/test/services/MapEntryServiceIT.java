package pl.edu.pw.elka.spdb.test.services;

import junit.framework.TestCase;
import org.apache.cxf.helpers.IOUtils;
import org.apache.cxf.jaxrs.client.WebClient;
import org.junit.Test;
import pl.edu.pw.elka.spdb.model.MapEntry;

import javax.ws.rs.core.Response;
import java.io.InputStream;

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

        assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
        assertEquals("{\"latitude\":52.220067,\"longitude\":21.012119}", content);
    }
}
