package pl.edu.pw.elka.spdb.dao.entries;

import pl.edu.pw.elka.spdb.model.MapEntry;

public interface IMapEntryDAO {
    MapEntry insertMapEntry(MapEntry mapEntry);
    MapEntry findMapEntryById(Long id);
}
