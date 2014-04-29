#import <MapKit/MapKit.h>
#import <UIKit/UIKit.h>

@interface SPDBMapRouteProjectionViewController : UIViewController<MKMapViewDelegate>

@property (nonatomic, strong) IBOutlet MKMapView *mapView;

@property (nonatomic, strong) IBOutlet NSArray *route;

@end
