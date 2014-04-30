package pl.edu.pw.elka.spdb.model;

import com.google.gson.annotations.Expose;
import org.springframework.data.neo4j.annotation.*;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import java.time.Duration;

@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
@RelationshipEntity(type = "ROUTES_TO")
public class Route {
    @GraphId
    @Expose
    private Long id;
    @StartNode
    @Expose
    private MapEntry routeFrom;
    @EndNode
    @Expose
    private MapEntry routeTo;
    @GraphProperty(propertyType = Long.class)
    @Expose
    private Duration duration;

    public Route() {
    }

    public Route(MapEntry routeFrom, MapEntry routeTo, Duration duration) {
        this.routeFrom = routeFrom;
        this.routeTo = routeTo;
        this.duration = duration;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public MapEntry getRouteFrom() {
        return routeFrom;
    }

    public void setRouteFrom(MapEntry routeFrom) {
        this.routeFrom = routeFrom;
    }

    public MapEntry getRouteTo() {
        return routeTo;
    }

    public void setRouteTo(MapEntry routeTo) {
        this.routeTo = routeTo;
    }

    public Duration getDuration() {
        return duration;
    }

    public void setDuration(Duration duration) {
        this.duration = duration;
    }
}
