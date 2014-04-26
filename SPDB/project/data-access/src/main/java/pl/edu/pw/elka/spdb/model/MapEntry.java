package pl.edu.pw.elka.spdb.model;

import org.springframework.data.neo4j.annotation.GraphId;
import org.springframework.data.neo4j.annotation.Indexed;
import org.springframework.data.neo4j.annotation.NodeEntity;
import org.springframework.data.neo4j.support.index.IndexType;

@NodeEntity
public class MapEntry {
    @GraphId
    private Long id;

    @Indexed(indexType = IndexType.POINT, indexName = "MapEntryLocation")
    private String wkt;

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

    public String toString() {
        return String.format("MapEntry of id: %1$s and location: %2$s", id, wkt);
    }

    @Override
    public boolean equals(Object object) {
        MapEntry other = (MapEntry) object;

        return id.equals(other.id) && wkt.equals(other.wkt);
    }
}
