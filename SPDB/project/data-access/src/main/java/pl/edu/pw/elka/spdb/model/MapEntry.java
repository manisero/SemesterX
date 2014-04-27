package pl.edu.pw.elka.spdb.model;

import org.springframework.data.neo4j.annotation.GraphId;
import org.springframework.data.neo4j.annotation.Indexed;
import org.springframework.data.neo4j.annotation.NodeEntity;
import org.springframework.data.neo4j.annotation.RelatedToVia;
import org.springframework.data.neo4j.support.index.IndexType;

import java.time.Duration;
import java.util.Collection;
import java.util.HashSet;

@NodeEntity
public class MapEntry {
    @GraphId
    private Long id;

    @Indexed(indexType = IndexType.POINT, indexName = "MapEntryLocation")
    private String wkt;

    @RelatedToVia
    private Collection<Route> routes = new HashSet<>();

    protected MapEntry() {
    }

    public MapEntry(double latitude, double longitude) {
        setLocation(latitude, longitude);
    }

    public Long getId() {
        return id;
    }

    public void setLocation(double latitude, double longitude) {
        wkt = String.format("POINT( %.2f %.2f )", latitude, longitude).replace(",", ".");
    }

    public Route addRoute(MapEntry mapEntry, Duration duration) {
        final Route route = new Route(this, mapEntry, duration);
        routes.add(route);

        return route;
    }

    public boolean routesTo(MapEntry mapEntry) {
        return routes.stream().anyMatch(route -> route.routeFrom.equals(this) && route.routeTo.equals(mapEntry));
    }

    public Duration getTravelTime(MapEntry mapEntry) {
        Route foundRoute = routes.stream().filter(route -> route.routeTo.equals(mapEntry)).findFirst().orElse(null);
        return foundRoute != null ? foundRoute.duration : null;
    }

    public String toString() {
        return String.format("MapEntry of id: %1$s and location: %2$s", id, wkt);
    }

    @Override
    public boolean equals(Object object) {
        MapEntry other = (MapEntry) object;

        if (id == null || other.id == null) {
            return false;
        }

        return id.equals(other.id);
    }
}
