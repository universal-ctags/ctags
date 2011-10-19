
@interface Person : NSObject {
    @public
        NSString *m_name;
    @private
        int m_age;
}
 
@property(copy) NSString *personName;
@property(readonly) int personAge;
 
-(id)initWithAge:(int)age;
@end
