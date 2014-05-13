package pl.edu.pw.elka.spdb.dao.entries.impl;

import org.neo4j.graphalgo.GraphAlgoFactory;
import org.neo4j.graphalgo.PathFinder;
import org.neo4j.graphalgo.WeightedPath;
import org.neo4j.graphdb.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.neo4j.conversion.EndResult;
import org.springframework.data.neo4j.support.Neo4jTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.edu.pw.elka.spdb.configuration.IConfigurationProvider;
import pl.edu.pw.elka.spdb.dao.entries.IMapEntryDAO;
import pl.edu.pw.elka.spdb.evaluators.PublicTransportCostEvaluator;
import pl.edu.pw.elka.spdb.model.MapEntry;
import pl.edu.pw.elka.spdb.model.PublicTransportRoute;
import pl.edu.pw.elka.spdb.model.Route;
import pl.edu.pw.elka.spdb.relationships.MapEntryRelationships;
import pl.edu.pw.elka.spdb.repositories.IMapEntryRepository;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

@Service
@Transactional
public class Neo4jMapEntryDAO implements IMapEntryDAO {
    @Autowired
    private IMapEntryRepository mapEntryRepository;
    @Autowired
    private Neo4jTemplate template;
    @Autowired
    private IConfigurationProvider configurationProvider;

    @Override
    public MapEntry insertMapEntry(MapEntry mapEntry) {
        return mapEntryRepository.save(mapEntry);
    }

    @Override
    public MapEntry findMapEntryById(Long id) {
        return mapEntryRepository.findOne(id);
    }

    @Override
    public List<Route> findFastestRoute(MapEntry start, MapEntry end) {
        List<Route> fastestRoute = new ArrayList<>();
        Node startNode = template.getNode(start.getId());
        Node endNode = template.getNode(end.getId());

        WeightedPath shortestPath = getShortestPath(startNode, endNode);

        if (shortestPath != null) {
            shortestPath.relationships().forEach(rel -> fastestRoute.add(findRouteBetween(rel.getStartNode().getId(),
                    rel.getEndNode().getId())));
        }

        return fastestRoute;
    }

    private WeightedPath getShortestPath(Node start, Node end) {
        PathFinder<WeightedPath> finder = GraphAlgoFactory.dijkstra(PathExpanders.forTypeAndDirection
                (DynamicRelationshipType.withName(MapEntryRelationships.ROUTES_TO.getValue()),
                        Direction.OUTGOING), "duration");

        return finder.findSinglePath(start, end);
    }

    @Override
    public MapEntry findNearestMapEntry(double latitude, double longitude) {
        EndResult<MapEntry> entriesWithinDistance = mapEntryRepository.findWithinDistance("MapEntryLocation", latitude,
                longitude, configurationProvider.getSearchRadius());

        Iterator<MapEntry> entryIterator = entriesWithinDistance.iterator();

        while (entryIterator.hasNext()) {
            return entryIterator.next();
        }

        return null;
    }

    @Override
    public Route findRouteBetween(MapEntry start, MapEntry end)
    {
        Route route = mapEntryRepository.getRelationshipBetween(start, end, Route.class,
                MapEntryRelationships.ROUTES_TO.getValue());

        if (route != null) {
            route.setRouteFrom(start);
            route.setRouteTo(end);
        }

        return route;
    }

    @Override
    public Route findRouteBetween(Long startId, Long endId)
    {
        return findRouteBetween(findMapEntryById(startId), findMapEntryById(endId));
    }

    @Override
    public MapEntry findNearestPublicTransportStop(double latitude, double longitude) {
        EndResult<MapEntry> entriesWithinDistance = mapEntryRepository.findWithinDistance("MapEntryLocation", latitude,
                longitude, configurationProvider.getSearchRadius());

        Iterator<MapEntry> entryIterator = entriesWithinDistance.iterator();

        while (entryIterator.hasNext()) {
            MapEntry entry = entryIterator.next();

            if (entry.getPublicTransportStop()) {
                return entry;
            }
        }

        return null;
    }

    @Override
    public List<PublicTransportRoute> findFastestPublicTransportRoute(MapEntry start, MapEntry end, Duration
            timeNeededToChange) {
        List<PublicTransportRoute> fastestRoute = new ArrayList<>();
        Node startNode = template.getNode(start.getId());
        Node endNode = template.getNode(end.getId());

        WeightedPath shortestPath = getShortestPublicTransportPath(startNode, endNode, timeNeededToChange);

        if (shortestPath != null) {
            shortestPath.relationships().forEach(rel -> fastestRoute.add(
                    findPublicTransportRouteBetween(rel.getStartNode().getId(), rel.getEndNode().getId())));
        }

        return fastestRoute;
    }

    private WeightedPath getShortestPublicTransportPath(Node start, Node end, Duration timeNeededToChange) {
        RelationshipType publicTransportRelationship = DynamicRelationshipType.withName(
                MapEntryRelationships.PUBLIC_TRANSPORT_TO.getValue());

        PathExpander expander = PathExpanders.forTypeAndDirection(publicTransportRelationship, Direction.OUTGOING);
        PathFinder<WeightedPath> finder = GraphAlgoFactory.dijkstra(expander,
                new PublicTransportCostEvaluator(timeNeededToChange));

        return finder.findSinglePath(start, end);
    }

    public PublicTransportRoute findPublicTransportRouteBetween(MapEntry start, MapEntry end) {
        PublicTransportRoute route = mapEntryRepository.getRelationshipBetween(start, end, PublicTransportRoute.class,
                MapEntryRelationships.PUBLIC_TRANSPORT_TO.getValue());

        if (route != null) {
            route.setRouteFrom(start);
            route.setRouteTo(end);
        }

        return route;
    }

    public PublicTransportRoute findPublicTransportRouteBetween(Long startId, Long endId) {
        return findPublicTransportRouteBetween(mapEntryRepository.findOne(startId), mapEntryRepository.findOne(endId));
    }
}
