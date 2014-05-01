package pl.edu.pw.elka.spdb.dao.entries;

import pl.edu.pw.elka.spdb.model.MapEntry;
import pl.edu.pw.elka.spdb.model.Route;

import java.util.List;

public interface IMapEntryDAO {
    MapEntry insertMapEntry(MapEntry mapEntry);
    MapEntry findMapEntryById(Long id);
    List<Route> findFastestRoute(MapEntry start, MapEntry end);
    MapEntry findNearestMapEntry(double latitude, double longitude);
    Route findRouteBetween(MapEntry start, MapEntry end);
    Route findRouteBetween(Long startId, Long endId);
}
