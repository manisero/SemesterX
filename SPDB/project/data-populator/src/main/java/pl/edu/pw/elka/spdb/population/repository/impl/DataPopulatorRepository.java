package pl.edu.pw.elka.spdb.population.repository.impl;

import pl.edu.pw.elka.spdb.model.MapEntry;
import pl.edu.pw.elka.spdb.model.Route;
import pl.edu.pw.elka.spdb.population.repository.IDataPopulatorRepository;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;

public class DataPopulatorRepository implements IDataPopulatorRepository {
    private List<MapEntry> entries;

    @Override
    public List<MapEntry> getEntries() {
        if (entries == null) {
            entries = new ArrayList<>();

            entries.add(new MapEntry(52.220067, 21.012119));
            entries.add(new MapEntry(52.220146, 21.004913));
        }

        return entries;
    }

    @Override
    public List<Route> getRoutes() {
        if (entries == null) {
            entries = getEntries();
        }

        List<Route> routes = new ArrayList<>();

        routes.add(routeBetweenEntries(0, 1, Duration.ofMinutes(3)));

        return routes;
    }

    private Route routeBetweenEntries(int indexFrom, int indexTo, Duration duration) {
        return entries.get(indexFrom).addRoute(entries.get(indexTo), duration);
    }
}
