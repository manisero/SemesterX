package pl.edu.pw.elka.spdb.population.repository.impl;

import pl.edu.pw.elka.spdb.coordinates.Coordinates;
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

            entries.add(new MapEntry(new Coordinates(52.220067, 21.012119)));
            entries.add(new MapEntry(new Coordinates(52.220146, 21.004913)));
            entries.add(new MapEntry(new Coordinates(52.223008, 21.004934)));
            entries.add(new MapEntry(new Coordinates(52.227885, 21.001865)));
            entries.add(new MapEntry(new Coordinates(52.230014, 21.011886)));
            entries.add(new MapEntry(new Coordinates(52.219893, 21.018152)));
            entries.add(new MapEntry(new Coordinates(52.223232, 21.015984)));
            entries.add(new MapEntry(new Coordinates(52.226229, 21.014161)));
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
        routes.add(routeBetweenEntries(1, 2, Duration.ofMinutes(2)));
        routes.add(routeBetweenEntries(2, 3, Duration.ofMinutes(4)));
        routes.add(routeBetweenEntries(3, 4, Duration.ofMinutes(5)));
        routes.add(routeBetweenEntries(0, 5, Duration.ofMinutes(2)));
        routes.add(routeBetweenEntries(5, 6, Duration.ofMinutes(2)));
        routes.add(routeBetweenEntries(6, 7, Duration.ofMinutes(2)));
        routes.add(routeBetweenEntries(7, 4, Duration.ofMinutes(4)));

        return routes;
    }

    private Route routeBetweenEntries(int indexFrom, int indexTo, Duration duration) {
        return entries.get(indexFrom).addRoute(entries.get(indexTo), duration);
    }
}
