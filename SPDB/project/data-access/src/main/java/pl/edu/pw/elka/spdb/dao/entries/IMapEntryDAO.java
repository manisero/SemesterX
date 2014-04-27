package pl.edu.pw.elka.spdb.dao.entries;

import pl.edu.pw.elka.spdb.model.MapEntry;

import java.util.List;

public interface IMapEntryDAO {
    MapEntry insertMapEntry(MapEntry mapEntry);
    MapEntry findMapEntryById(Long id);
    List<MapEntry> findFastestRoute(MapEntry start, MapEntry end);
    MapEntry findNearestMapEntry(double latitude, double longitude);
}
