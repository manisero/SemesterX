package pl.edu.pw.elka.spdb.population.repository.impl;

import org.springframework.beans.factory.annotation.Autowired;
import pl.edu.pw.elka.spdb.model.MapEntry;
import pl.edu.pw.elka.spdb.model.PublicTransportRoute;
import pl.edu.pw.elka.spdb.model.Route;
import pl.edu.pw.elka.spdb.population.reader.entry.IStreamMapEntryReader;
import pl.edu.pw.elka.spdb.population.repository.IDataPopulatorRepository;

import java.io.InputStream;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class StreamDataPopulatorRepository implements IDataPopulatorRepository {
    private final InputStream entriesStream;
    private final InputStream routesStream;
    private final InputStream publicTransportRoutesStream;

    @Autowired
    private IStreamMapEntryReader streamMapEntryReader;

    private Map<String, MapEntry> entries;
    private Map<String, Route> routes;
    private Map<String, PublicTransportRoute> publicTransportRoutes;

    public StreamDataPopulatorRepository(InputStream entriesStream, InputStream routesStream,
                                         InputStream publicTransportRoutesStream) {
        this.entriesStream = entriesStream;
        this.routesStream = routesStream;
        this.publicTransportRoutesStream = publicTransportRoutesStream;
    }

    @Override
    public List<MapEntry> getEntries() {
        if (entries.isEmpty()) {
            entries = streamMapEntryReader.readEntries(entriesStream);
        }

        return entries.entrySet().stream().map(e -> e.getValue()).collect(Collectors.toList());
    }

    @Override
    public List<Route> getRoutes() {
        return null;
    }

    @Override
    public List<PublicTransportRoute> getPublicTransportRoutes() {
        return null;
    }
}
