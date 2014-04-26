package pl.edu.pw.elka.spdb.model;

import org.springframework.data.neo4j.annotation.EndNode;
import org.springframework.data.neo4j.annotation.GraphId;
import org.springframework.data.neo4j.annotation.RelationshipEntity;
import org.springframework.data.neo4j.annotation.StartNode;

@RelationshipEntity(type = "ROUTES_TO")
public class Route {
    @GraphId Long id;
    @StartNode MapEntry routeFrom;
    @EndNode MapEntry routeTo;

    protected Route() {
    }

    public Route(MapEntry routeFrom, MapEntry routeTo) {
        this.routeFrom = routeFrom;
        this.routeTo = routeTo;
    }
}
