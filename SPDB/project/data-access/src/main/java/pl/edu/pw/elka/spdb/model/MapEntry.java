package pl.edu.pw.elka.spdb.model;

import org.springframework.data.neo4j.annotation.GraphId;
import org.springframework.data.neo4j.annotation.Indexed;
import org.springframework.data.neo4j.annotation.NodeEntity;
import org.springframework.data.neo4j.annotation.RelatedToVia;
import org.springframework.data.neo4j.support.index.IndexType;
import pl.edu.pw.elka.spdb.coordinates.Coordinates;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import java.time.Duration;
import java.util.Collection;
import java.util.HashSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@XmlRootElement(name = "")
@XmlAccessorType(XmlAccessType.PROPERTY)
@XmlType(propOrder = {})
@NodeEntity
public class MapEntry {
    @GraphId
    private Long id;

    @Indexed(indexType = IndexType.POINT, indexName = "MapEntryLocation")
    private String wkt;

    @RelatedToVia
    private Collection<Route> routes = new HashSet<>();

    public MapEntry() {
    }

    public MapEntry(Coordinates coordinates) {
        setCoordinates(coordinates);
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getWkt() {
        return wkt;
    }

    public void setWkt(String wkt) {
        this.wkt = wkt;
    }

    public Coordinates getCoordinates() {
        Pattern pattern = Pattern.compile("POINT\\(\\s(\\S+)\\s(\\S+)\\s\\)");
        Matcher matcher = pattern.matcher(this.wkt);

        if (matcher.matches()) {
            String latitude = matcher.group(1);
            String longitude = matcher.group(2);

            return new Coordinates(Double.valueOf(latitude), Double.valueOf(longitude));
        }

        return null;
    }

    public void setCoordinates(Coordinates coordinates) {
        String dotSeparatedWkt = String.format("POINT( %.8f %.8f )", coordinates.getLatitude(),
                coordinates.getLongitude());

        wkt = dotSeparatedWkt.replace(",", ".");
    }

    public Route addRoute(MapEntry mapEntry, Duration duration) {
        final Route route = new Route(this, mapEntry, duration);
        routes.add(route);

        return route;
    }

    public boolean routesTo(MapEntry mapEntry) {
        return routes.stream().anyMatch(route -> route.getRouteFrom().equals(this) && route.getRouteTo().equals
                (mapEntry));
    }

    public Duration getTravelTime(MapEntry mapEntry) {
        Route foundRoute = routes.stream().filter(route -> route.getRouteTo().equals(mapEntry)).findFirst().orElse
                (null);

        return foundRoute != null ? foundRoute.getDuration() : null;
    }

    public String toString() {
        return String.format("MapEntry of id: %1$s and location: %2$s", id, wkt);
    }

    @Override
    public boolean equals(Object object) {
        MapEntry other = (MapEntry) object;

        return !(id == null || other.id == null) && id.equals(other.id);
    }
}
