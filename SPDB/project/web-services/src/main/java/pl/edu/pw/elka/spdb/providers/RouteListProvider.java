package pl.edu.pw.elka.spdb.providers;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.reflect.TypeToken;
import org.apache.cxf.helpers.IOUtils;
import pl.edu.pw.elka.spdb.adapters.DurationTypeAdapter;
import pl.edu.pw.elka.spdb.adapters.RouteListAdapter;
import pl.edu.pw.elka.spdb.model.Route;

import javax.ws.rs.Consumes;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.ext.MessageBodyReader;
import javax.ws.rs.ext.MessageBodyWriter;
import javax.ws.rs.ext.Provider;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.annotation.Annotation;
import java.lang.reflect.Type;
import java.time.Duration;
import java.util.List;

@Consumes("application/json")
@Produces("application/json")
@Provider
public class RouteListProvider implements MessageBodyWriter<RouteListAdapter>, MessageBodyReader<RouteListAdapter> {
    @Override
    public boolean isWriteable(Class<?> aClass, Type type, Annotation[] annotations, MediaType mediaType) {
        return aClass.equals(RouteListAdapter.class);
    }

    @Override
    public long getSize(RouteListAdapter route, Class<?> aClass, Type type, Annotation[] annotations, MediaType mediaType) {
        return -1;
    }

    @Override
    public void writeTo(RouteListAdapter route, Class<?> aClass, Type type, Annotation[] annotations, MediaType mediaType,
                        MultivaluedMap<String, Object> stringObjectMultivaluedMap, OutputStream outputStream) throws
            IOException, WebApplicationException {
        Gson gson = new GsonBuilder().excludeFieldsWithoutExposeAnnotation().registerTypeAdapter(Duration.class,
                new DurationTypeAdapter()).create();
        String routeAsJson = gson.toJson(route.getRoutes(), new TypeToken<List<Route>>(){}.getType());

        outputStream.write(routeAsJson.getBytes());
    }

    @Override
    public boolean isReadable(Class<?> aClass, Type type, Annotation[] annotations, MediaType mediaType) {
        return aClass.equals(RouteListAdapter.class);
    }

    @Override
    public RouteListAdapter readFrom(Class<RouteListAdapter> routeListAdapterClass, Type type,
                                     Annotation[] annotations, MediaType mediaType,
                                     MultivaluedMap<String, String> stringStringMultivaluedMap,
                                     InputStream inputStream) throws IOException, WebApplicationException {
        String json = IOUtils.toString(inputStream);
        Gson gson = new GsonBuilder().excludeFieldsWithoutExposeAnnotation().registerTypeAdapter(Duration.class,
                new DurationTypeAdapter()).create();
        List<Route> routesFromJson = gson.fromJson(json, new TypeToken<List<Route>>() {
        }.getType());

        return new RouteListAdapter(routesFromJson);
    }
}
