#import "SPDBMapSelectionDelegate.h"
#import <UIKit/UIKit.h>

@interface SPDBRootViewController : UIViewController<UITableViewDataSource, UITableViewDelegate, SPDBMapSelectionDelegate>

@property (nonatomic, strong) IBOutlet UITableView *tableView;

@property (nonatomic, strong) NSValue *pointFrom;
@property (nonatomic, strong) NSValue *pointTo;
@property (nonatomic, strong) NSValue *selectedPoint;

@end
