package pl.edu.pw.elka.spdb.relationships;

public enum MapEntryRelationships {
    ROUTES_TO("ROUTES_TO");

    private String value;

    private MapEntryRelationships(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }
}
