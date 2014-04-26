package pl.edu.pw.elka.spdb.dao.location;

import pl.edu.pw.elka.spdb.model.Location;

public interface ILocationDAO {
    void insertLocation(Location location);
    Location findLocation(Location location);
}
