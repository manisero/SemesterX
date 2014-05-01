#import "SPDBDateUtilities.h"
#import "SPDBMapEntry.h"
#import "SPDBMapRouteProjectionViewController.h"
#import "SPDBRoute.h"

@interface SPDBMapRouteProjectionViewController ()

@end

@implementation SPDBMapRouteProjectionViewController

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    return self;
}

- (void)viewDidLoad
{
    [super viewDidLoad];
    [self setMapRegionToWarsaw];
    [self drawRoute];
    [self annotateRouteDetails];
}

- (void)setMapRegionToWarsaw
{
    MKCoordinateRegion warsaw = MKCoordinateRegionMake(CLLocationCoordinate2DMake(52.232091, 21.006154), MKCoordinateSpanMake(0.04, 0.3));
    [self.mapView setRegion:warsaw animated:YES];
}

- (void)drawRoute
{
    NSArray *coordinatesAsObjects = [self convertRouteToCoordinates:self.route];
    int routeLength = [coordinatesAsObjects count];
    CLLocationCoordinate2D coordinates[routeLength];
    
    for (int i = 0; i < routeLength; ++i)
    {
        CLLocation *location = [coordinatesAsObjects objectAtIndex:i];
        coordinates[i] = [location coordinate];
    }
    
    MKPolyline *polyline = [MKPolyline polylineWithCoordinates:coordinates count:routeLength];
    [self.mapView removeOverlays:self.mapView.overlays];
    [self.mapView addOverlay:polyline];
}

- (NSArray *)convertRouteToCoordinates:(NSArray *)route
{
    NSMutableArray *coordinates = [NSMutableArray new];
    
    for (int i = 0; i < [route count]; ++i)
    {
        SPDBRoute *routeSegment = [route objectAtIndex:i];
        [coordinates addObject:[self mapEntryToLocation:routeSegment.routeFrom]];
    }
    
    if ([route count] > 0)
    {
        SPDBRoute *lastSegment = [route lastObject];
        [coordinates addObject:[self mapEntryToLocation:lastSegment.routeTo]];
    }
    
    return coordinates;
}

- (CLLocation *)mapEntryToLocation:(SPDBMapEntry *)mapEntry
{
    return [[CLLocation alloc] initWithLatitude:[mapEntry.latitude doubleValue]
                                      longitude:[mapEntry.longitude doubleValue]];
}

- (void)annotateRouteDetails
{
    if ([self.route count] > 0)
    {
        [self annotateRouteStart];
        [self annotateRouteEnd];
    }
}

- (void)annotateRouteStart
{
    SPDBRoute *firstSegment = [self.route firstObject];
    SPDBMapEntry *routeStart = firstSegment.routeFrom;
    
    MKPointAnnotation *pointAnnotation = [MKPointAnnotation new];
    [pointAnnotation setCoordinate:CLLocationCoordinate2DMake([routeStart.latitude doubleValue], [routeStart.longitude doubleValue])];
    [pointAnnotation setTitle:@"Route start"];

    SPDBDateUtilities *dateUtilities = [SPDBDateUtilities new];
    NSString *subtitle = [NSString stringWithFormat:@"Set off time: %@", [dateUtilities formatDate:[self calculateSetOffTime] withFormat:@"HH:mm:ss"]];
                          
    [pointAnnotation setSubtitle:subtitle];
    
    [self.mapView addAnnotation:pointAnnotation];
    [self.mapView selectAnnotation:pointAnnotation animated:YES];
}

- (void)annotateRouteEnd
{
    SPDBRoute *lastSegment = [self.route lastObject];
    SPDBMapEntry *routeEnd = lastSegment.routeTo;
    MKPointAnnotation *pointAnnotation = [MKPointAnnotation new];
    [pointAnnotation setCoordinate:CLLocationCoordinate2DMake([routeEnd.latitude doubleValue], [routeEnd.longitude doubleValue])];
    [pointAnnotation setTitle:@"Route end"];
    
    SPDBDateUtilities *dateUtilities = [SPDBDateUtilities new];
    NSString *subtitle = [NSString stringWithFormat:@"Arrival time: %@", [dateUtilities formatDate:[dateUtilities stripSecondsFromDate:self.arrivalTime] withFormat:@"HH:mm:ss"]];
    
    [pointAnnotation setSubtitle:subtitle];
    
    [self.mapView addAnnotation:pointAnnotation];
}

- (NSDate *)calculateSetOffTime
{
    long travelDurationInSeconds = 0;
    
    for (int i = 0; i < [self.route count]; ++i)
    {
        SPDBRoute *routeSegment = [self.route objectAtIndex:i];
        travelDurationInSeconds += [[routeSegment duration] longValue];
    }
    
    SPDBDateUtilities *dateUtilities = [SPDBDateUtilities new];
    NSDate *arrivalTime = [dateUtilities stripSecondsFromDate:self.arrivalTime];
    NSDate *setOffTime = [dateUtilities subtractSeconds:travelDurationInSeconds fromDate:arrivalTime];
    
    return setOffTime;
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
}

- (MKOverlayRenderer *)mapView:(MKMapView *)mapView rendererForOverlay:(id<MKOverlay>)overlay
{
    if([overlay isKindOfClass:[MKPolyline class]])
    {
        MKPolylineRenderer *polylineRenderer = [[MKPolylineRenderer alloc] initWithPolyline:overlay];
        polylineRenderer.strokeColor = [[UIColor colorWithRed:0.5 green:0.0 blue:0.5 alpha:1.0] colorWithAlphaComponent:1.0];
        polylineRenderer.lineWidth = 5.0;
        return polylineRenderer;
    }
    else
    {
        return  nil;
    }
}

- (MKAnnotationView *)mapView:(MKMapView *)mapView viewForAnnotation:(id <MKAnnotation>)annotation
{
    static NSString *annotationIdentifier = @"Annotation";
    
    MKPinAnnotationView *pinView = (MKPinAnnotationView *) [mapView
                                                            dequeueReusableAnnotationViewWithIdentifier:annotationIdentifier];
    
    if (!pinView)
    {
        pinView = [[MKPinAnnotationView alloc]
                        initWithAnnotation:annotation
                        reuseIdentifier:annotationIdentifier];
        
        if ([[annotation title] isEqualToString:@"Route start"])
        {
            [pinView setPinColor:MKPinAnnotationColorGreen];
        }
        else if ([[annotation title] isEqualToString:@"Route end"])
        {
            [pinView setPinColor:MKPinAnnotationColorRed];
        }
        
        pinView.animatesDrop = YES;
        pinView.canShowCallout = YES;
    }
    else 
    {
        pinView.annotation = annotation;
    }
    
    return pinView; 
}

@end
