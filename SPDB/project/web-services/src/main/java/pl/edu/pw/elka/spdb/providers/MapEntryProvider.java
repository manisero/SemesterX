package pl.edu.pw.elka.spdb.providers;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import pl.edu.pw.elka.spdb.model.MapEntry;

import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.ext.MessageBodyWriter;
import javax.ws.rs.ext.Provider;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.annotation.Annotation;
import java.lang.reflect.Type;

@Produces("application/json")
@Provider
public class MapEntryProvider implements MessageBodyWriter<MapEntry> {
    @Override
    public boolean isWriteable(Class<?> aClass, Type type, Annotation[] annotations, MediaType mediaType) {
        return aClass.equals(MapEntry.class);
    }

    @Override
    public long getSize(MapEntry mapEntry, Class<?> aClass, Type type, Annotation[] annotations, MediaType mediaType) {
        return -1;
    }

    @Override
    public void writeTo(MapEntry mapEntry, Class<?> aClass, Type type, Annotation[] annotations, MediaType mediaType,
                        MultivaluedMap<String, Object> stringObjectMultivaluedMap, OutputStream outputStream) throws
            IOException, WebApplicationException {
        Gson gson = new GsonBuilder().excludeFieldsWithoutExposeAnnotation().create();
        String mapEntryAsJson = gson.toJson(mapEntry);

        outputStream.write(mapEntryAsJson.getBytes());
    }
}
