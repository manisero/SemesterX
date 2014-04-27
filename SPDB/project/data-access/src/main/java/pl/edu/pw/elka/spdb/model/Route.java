package pl.edu.pw.elka.spdb.model;

import org.springframework.data.neo4j.annotation.*;
import pl.edu.pw.elka.spdb.relationships.MapEntryRelationships;

import java.time.Duration;

@RelationshipEntity(type = "ROUTES_TO")
public class Route {
    @GraphId Long id;
    @StartNode MapEntry routeFrom;
    @EndNode MapEntry routeTo;
    @GraphProperty(propertyType = Long.class) Duration duration;

    protected Route() {
    }

    public Route(MapEntry routeFrom, MapEntry routeTo, Duration duration) {
        this.routeFrom = routeFrom;
        this.routeTo = routeTo;
        this.duration = duration;
    }
}
