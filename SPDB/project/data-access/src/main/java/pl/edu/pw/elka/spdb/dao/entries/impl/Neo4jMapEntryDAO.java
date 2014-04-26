package pl.edu.pw.elka.spdb.dao.entries.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.edu.pw.elka.spdb.dao.entries.IMapEntryDAO;
import pl.edu.pw.elka.spdb.model.MapEntry;
import pl.edu.pw.elka.spdb.repositories.IMapEntryRepository;

@Service
@Transactional
public class Neo4jMapEntryDAO implements IMapEntryDAO {
    @Autowired private IMapEntryRepository mapEntryRepository;

    @Override
    public MapEntry insertMapEntry(MapEntry mapEntry) {
        return mapEntryRepository.save(mapEntry);
    }

    @Override
    public MapEntry findMapEntryById(Long id) {
        return mapEntryRepository.findOne(id);
    }
}
