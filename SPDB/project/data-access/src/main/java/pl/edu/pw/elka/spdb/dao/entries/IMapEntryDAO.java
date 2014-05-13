package pl.edu.pw.elka.spdb.dao.entries;

import pl.edu.pw.elka.spdb.model.MapEntry;
import pl.edu.pw.elka.spdb.model.PublicTransportRoute;
import pl.edu.pw.elka.spdb.model.Route;

import java.time.Duration;
import java.util.List;

public interface IMapEntryDAO {
    MapEntry insertMapEntry(MapEntry mapEntry);
    MapEntry findMapEntryById(Long id);
    List<Route> findFastestRoute(MapEntry start, MapEntry end);
    MapEntry findNearestMapEntry(double latitude, double longitude);
    Route findRouteBetween(MapEntry start, MapEntry end);
    Route findRouteBetween(Long startId, Long endId);
    MapEntry findNearestPublicTransportStop(double latitude, double longitude);
    List<PublicTransportRoute> findFastestPublicTransportRoute(MapEntry start, MapEntry end,
                                                               Duration timeNeededToChange);
    PublicTransportRoute findPublicTransportRouteBetween(MapEntry start, MapEntry end);
    PublicTransportRoute findPublicTransportRouteBetween(Long startId, Long endId);
}
