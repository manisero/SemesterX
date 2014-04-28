package pl.edu.pw.elka.spdb.services;

import org.springframework.beans.factory.annotation.Autowired;
import pl.edu.pw.elka.spdb.dao.entries.IMapEntryDAO;
import pl.edu.pw.elka.spdb.model.MapEntry;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;
import java.util.List;

@Path("/entry")
public class MapEntryService {
    @Autowired
    private IMapEntryDAO mapEntryDAO;

    @GET
    @Path("/nearest/{latitude}/{longitude}")
    @Produces("application/json")
    public Response getNearestMapEntries(@PathParam("latitude") double latitude,
                                         @PathParam("longitude") double longitude) {
        MapEntry nearestMapEntry = mapEntryDAO.findNearestMapEntry(latitude, longitude);

        return Response.ok().entity(nearestMapEntry).build();
    }

    @GET
    @Path("/shortestPath/{startingNodeId}/{finishingNodeId}")
    @Produces("application/json")
    public Response getShortestPath(@PathParam("startingNodeId") long startingNodeId,
                                    @PathParam("finishingNodeId") long finishingNodeId) {
        MapEntry startingNode = mapEntryDAO.findMapEntryById(startingNodeId);
        MapEntry finishingNode = mapEntryDAO.findMapEntryById(finishingNodeId);
        List<MapEntry> shortestPath = mapEntryDAO.findFastestRoute(startingNode, finishingNode);

        return Response.ok().entity(shortestPath).build();
    }
}
